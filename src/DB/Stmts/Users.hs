{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module DB.Stmts.Users where

import qualified Data.ByteString as Bs
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)

import Data.Aeson (Value)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

-- NOTE
-- ----
-- This module provides the DB statements (Hasql-TH quasiquotes) required by:
--   * /wbap/auth/login, /logout, /renew
--   * /wbap/stream?sid=...
--   * /wbap/user/me, /sessions, /sessions/:sidToRevoke
--   * /wbap/authz/me
--   * /wbap/admin/users, /wapps/:wappId/members
--
-- Password hashing/verification is performed in Haskell. The DB only stores:
--   ewuser.password_credentials.password_hash (PHC string; Argon2id recommended)
--
-- The schema is ewuser (no quoting required).


-- ==========================================================================
-- Login: lookup user by ident (email OR handle)
-- ==========================================================================

-- | Returns:
--   (user_id, status_text, display_name, password_hash)
loginLookupByIdent :: Statement Text (Maybe (UUID, Text, Maybe Text, Maybe Text))
loginLookupByIdent =
  [TH.maybeStatement|
    select
      u.id :: uuid,
      u.status :: text,
      u.display_name :: text?,
      pc.password_hash :: text?
    from ewuser.users u
    left join ewuser.user_emails e
      on e.user_id = u.id
      and e.is_primary = true
    left join ewuser.password_credentials pc
      on pc.user_id = u.id
    where
      (e.email = ($1 :: text)::citext)
      or (u.handle = $1 :: text)
    limit 1
  |]


-- ==========================================================================
-- Context + session creation (login / resume)
-- ==========================================================================

-- | Create a new context for (user_id, wapp_id).
insertContext :: Statement (UUID, UUID) UUID
insertContext =
  [TH.singletonStatement|
    insert into ewuser.contexts (user_id, wapp_id)
    values ($1 :: uuid, $2 :: uuid)
    returning id :: uuid
  |]

-- | Choose an existing context if provided and valid; otherwise create a new one;
--   then create a new session attached to that context.
--
-- Input:
--   (user_id, wapp_id, resume_context_id?, session_kind_text, user_agent?)
-- Output:
--   (session_id, context_id)
createSessionWithOptionalResumeContext :: Statement (UUID, UUID, Maybe UUID, Text, Maybe Text) (UUID, UUID)
createSessionWithOptionalResumeContext =
  [TH.singletonStatement|
    with existing as (
      select c.id :: uuid
      from ewuser.contexts c
      where c.id = $3 :: uuid?
        and c.user_id = $1 :: uuid
        and c.wapp_id = $2 :: uuid
        and c.status = 'active'
      limit 1
    ),
    created as (
      insert into ewuser.contexts (user_id, wapp_id)
      select $1 :: uuid, $2 :: uuid
      where not exists (select 1 from existing)
      returning id :: uuid
    ),
    chosen as (
      select id :: uuid from existing
      union all
      select id :: uuid from created
      limit 1
    ),
    ses as (
      insert into ewuser.sessions (user_id, context_id, kind, user_agent)
      select
        $1 :: uuid,
        chosen.id :: uuid,
        (($4 :: text)::ewuser.session_kind),
        $5 :: text?
      from chosen
      returning id :: uuid, context_id :: uuid
    )
    select ses.id :: uuid, ses.context_id :: uuid
    from ses
  |]


-- ==========================================================================
-- Session lifecycle: resolve/renew/logout
-- ==========================================================================

-- | Resolve a session for WebSocket usage.
--   Updates last_seen_at and returns:
--     (user_id, context_id, wapp_id)
--   Fails (Nothing) if session is revoked or has no context.
resolveSessionForWs :: Statement UUID (Maybe (UUID, UUID, UUID))
resolveSessionForWs =
  [TH.maybeStatement|
    with s as (
      update ewuser.sessions
      set last_seen_at = now()
      where id = $1 :: uuid
        and revoked_at is null
        and context_id is not null
      returning user_id :: uuid, context_id :: uuid
    )
    select
      s.user_id :: uuid,
      s.context_id :: uuid,
      c.wapp_id :: uuid
    from s
    join ewuser.contexts c on c.id = s.context_id
  |]

-- | Revoke (logout) the given session. Returns True if something was revoked.
revokeSessionById :: Statement UUID Bool
revokeSessionById =
  [TH.singletonStatement|
    with x as (
      update ewuser.sessions
      set revoked_at = now()
      where id = $1 :: uuid
        and revoked_at is null
      returning 1 :: int4
    )
    select coalesce((select true from x limit 1), false) :: bool
  |]

-- | Rotate a session (renew): revoke old sid and mint a new sid attached to the same context.
-- Input:
--   (old_session_id, session_kind_text, user_agent?)
-- Output:
--   (new_session_id, context_id)
renewSessionRotate :: Statement (UUID, Text, Maybe Text) (Maybe (UUID, UUID))
renewSessionRotate =
  [TH.maybeStatement|
    with old as (
      update ewuser.sessions
      set revoked_at = now()
      where id = $1 :: uuid
        and revoked_at is null
      returning user_id :: uuid, context_id :: uuid
    ),
    ins as (
      insert into ewuser.sessions (user_id, context_id, kind, user_agent)
      select
        old.user_id :: uuid,
        old.context_id :: uuid,
        (($2 :: text)::ewuser.session_kind),
        $3 :: text?
      from old
      where old.context_id is not null
      returning id :: uuid, context_id :: uuid
    )
    select ins.id :: uuid, ins.context_id :: uuid
    from ins
  |]


-- ==========================================================================
-- Session -> User resolution for REST endpoints
-- ==========================================================================

-- | Resolve an active session to (user_id, context_id?, wapp_id?).
resolveSessionPrincipal :: Statement UUID (Maybe (UUID, Maybe UUID, Maybe UUID))
resolveSessionPrincipal =
  [TH.maybeStatement|
    select
      s.user_id :: uuid,
      s.context_id :: uuid?,
      c.wapp_id :: uuid?
    from ewuser.sessions s
    left join ewuser.contexts c on c.id = s.context_id
    where s.id = $1 :: uuid
      and s.revoked_at is null
    limit 1
  |]


-- ==========================================================================
-- /wbap/user/me
-- ==========================================================================

-- | Fetch self profile by active sid.
-- Returns:
--   (user_id, primary_email?, display_name?, avatar_url?)
getUserSelfBySession :: Statement UUID (Maybe (UUID, Maybe Text, Maybe Text, Maybe Text))
getUserSelfBySession =
  [TH.maybeStatement|
    select
      u.id :: uuid,
      (e.email :: text) :: text?,
      u.display_name :: text?,
      p.avatar_url :: text?
    from ewuser.sessions s
    join ewuser.users u on u.id = s.user_id
    left join ewuser.user_emails e
      on e.user_id = u.id
      and e.is_primary = true
    left join ewuser.user_profile p
      on p.user_id = u.id
    where s.id = $1 :: uuid
      and s.revoked_at is null
    limit 1
  |]

-- | Update display_name (users table) by sid.
-- Input: (sid, display_name?)
-- Output: user_id (if sid valid)
updateSelfDisplayNameBySession :: Statement (UUID, Maybe Text) (Maybe UUID)
updateSelfDisplayNameBySession =
  [TH.maybeStatement|
    with me as (
      select user_id :: uuid
      from ewuser.sessions
      where id = $1 :: uuid
        and revoked_at is null
      limit 1
    ),
    upd as (
      update ewuser.users u
      set display_name = coalesce($2 :: text?, u.display_name)
      from me
      where u.id = me.user_id
      returning u.id :: uuid
    )
    select upd.id :: uuid from upd
  |]

-- | Upsert avatar_url into user_profile by sid.
-- Input: (sid, avatar_url?)
-- Output: user_id (if sid valid)
upsertSelfAvatarBySession :: Statement (UUID, Maybe Text) (Maybe UUID)
upsertSelfAvatarBySession =
  [TH.maybeStatement|
    with me as (
      select user_id :: uuid
      from ewuser.sessions
      where id = $1 :: uuid
        and revoked_at is null
      limit 1
    ),
    ins as (
      insert into ewuser.user_profile (user_id, avatar_url)
      select me.user_id :: uuid, $2 :: text?
      from me
      on conflict (user_id)
      do update set avatar_url = excluded.avatar_url, updated_at = now()
      returning user_id :: uuid
    )
    select ins.user_id :: uuid from ins
  |]


-- ==========================================================================
-- /wbap/user/sessions
-- ==========================================================================

-- | List sessions for the current user, identified by an active sid.
-- Returns rows:
--   (sid, cid?, created_at_text, last_seen_at_text, user_agent?, ip_text?)
listSessionsForSelfBySession :: Statement UUID (Vector (UUID, Maybe UUID, Text, Text, Maybe Text, Maybe Text))
listSessionsForSelfBySession =
  [TH.vectorStatement|
    with me as (
      select user_id :: uuid
      from ewuser.sessions
      where id = $1 :: uuid
        and revoked_at is null
      limit 1
    )
    select
      s.id :: uuid,
      s.context_id :: uuid?,
      to_char(s.created_at, 'YYYY-MM-DD"T"HH24:MI:SSOF') :: text,
      to_char(s.last_seen_at, 'YYYY-MM-DD"T"HH24:MI:SSOF') :: text,
      s.user_agent :: text?,
      (case when s.ip_addr is null then null else host(s.ip_addr) end) :: text?
    from ewuser.sessions s
    join me on me.user_id = s.user_id
    order by s.created_at desc
  |]

-- | Revoke another session (sidToRevoke) belonging to the same user as the caller sid.
-- Input: (caller_sid, target_sid)
-- Output: True if revoked.
revokeOtherSessionForSelf :: Statement (UUID, UUID) Bool
revokeOtherSessionForSelf =
  [TH.singletonStatement|
    with me as (
      select user_id :: uuid
      from ewuser.sessions
      where id = $1 :: uuid
        and revoked_at is null
      limit 1
    ),
    x as (
      update ewuser.sessions s
      set revoked_at = now()
      from me
      where s.id = $2 :: uuid
        and s.user_id = me.user_id
        and s.revoked_at is null
      returning 1 :: int4
    )
    select coalesce((select true from x limit 1), false) :: bool
  |]


-- ==========================================================================
-- /wbap/authz/me (RBAC expansion)
-- ==========================================================================

-- | Compute effective roles and permissions for a user at platform scope.
-- Output: (roles[], permissions[])
permsForUserPlatform :: Statement UUID (Vector Text, Vector Text)
permsForUserPlatform =
  [TH.singletonStatement|
    with role_ids as (
      select ur.role_id :: uuid
      from ewuser.user_roles ur
      join ewuser.roles r on r.id = ur.role_id
      where ur.user_id = $1 :: uuid
        and r.scope = 'platform'
    ),
    roles as (
      select r.name :: text
      from role_ids ri
      join ewuser.roles r on r.id = ri.role_id
    ),
    perms as (
      select p.name :: text
      from role_ids ri
      join ewuser.role_permissions rp on rp.role_id = ri.role_id
      join ewuser.permissions p on p.id = rp.permission_id
    )
    select
      coalesce((select array_agg(distinct roles.name) from roles), '{}'::text[]) :: text[],
      coalesce((select array_agg(distinct perms.name) from perms), '{}'::text[]) :: text[]
  |]

-- | Compute effective roles and permissions for a user within a wapp.
-- Includes platform roles + wapp membership role.
-- Output: (roles[], permissions[])
permsForUserAndWapp :: Statement (UUID, UUID) (Vector Text, Vector Text)
permsForUserAndWapp =
  [TH.singletonStatement|
    with role_ids as (
      select ur.role_id :: uuid
      from ewuser.user_roles ur
      join ewuser.roles r on r.id = ur.role_id
      where ur.user_id = $1 :: uuid
        and r.scope = 'platform'

      union

      select wm.role_id :: uuid
      from ewuser.wapp_memberships wm
      join ewuser.roles r on r.id = wm.role_id
      where wm.user_id = $1 :: uuid
        and wm.wapp_id = $2 :: uuid
        and r.scope = 'wapp'
        and wm.status = 'active'
    ),
    roles as (
      select r.name :: text
      from role_ids ri
      join ewuser.roles r on r.id = ri.role_id
    ),
    perms as (
      select p.name :: text
      from role_ids ri
      join ewuser.role_permissions rp on rp.role_id = ri.role_id
      join ewuser.permissions p on p.id = rp.permission_id
    )
    select
      coalesce((select array_agg(distinct roles.name) from roles), '{}'::text[]) :: text[],
      coalesce((select array_agg(distinct perms.name) from perms), '{}'::text[]) :: text[]
  |]


-- ==========================================================================
-- Admin: users
-- ==========================================================================

-- | List all users.
-- Returns (user_id, primary_email?, display_name?, status_text)
adminListUsers :: Statement () (Vector (UUID, Maybe Text, Maybe Text, Text))
adminListUsers =
  [TH.vectorStatement|
    select
      u.id :: uuid,
      (e.email :: text) :: text?,
      u.display_name :: text?,
      u.status :: text
    from ewuser.users u
    left join ewuser.user_emails e
      on e.user_id = u.id
      and e.is_primary = true
    order by u.created_at desc
  |]

-- | Create a user + primary email (unverified) in one statement.
-- Prevents orphan users when email already exists.
-- Input: (email, display_name?)
-- Output: new_user_id? (Nothing if email already exists)
adminCreateUserWithEmail :: Statement (Text, Maybe Text) (Maybe UUID)
adminCreateUserWithEmail =
  [TH.maybeStatement|
    with existing as (
      select 1 :: int4
      from ewuser.user_emails
      where email = ($1 :: text)::citext
      limit 1
    ),
    u as (
      insert into ewuser.users (display_name)
      select $2 :: text?
      where not exists (select 1 from existing)
      returning id :: uuid
    ),
    e as (
      insert into ewuser.user_emails (user_id, email, is_primary, is_verified)
      select u.id :: uuid, ($1 :: text)::citext, true, false
      from u
      returning user_id :: uuid
    )
    select u.id :: uuid
    from u
  |]

-- | Set (or replace) the password credential for a user.
-- Input: (user_id, password_hash, must_change)
adminUpsertPasswordCredential :: Statement (UUID, Text, Bool) ()
adminUpsertPasswordCredential =
  [TH.resultlessStatement|
    insert into ewuser.password_credentials (user_id, password_hash, must_change)
    values ($1 :: uuid, $2 :: text, $3 :: bool)
    on conflict (user_id)
    do update set
      password_hash = excluded.password_hash,
      must_change = excluded.must_change,
      password_updated_at = now()
  |]

-- | Get a user summary by id.
adminGetUser :: Statement UUID (Maybe (UUID, Maybe Text, Maybe Text, Text))
adminGetUser =
  [TH.maybeStatement|
    select
      u.id :: uuid,
      (e.email :: text) :: text?,
      u.display_name :: text?,
      u.status :: text
    from ewuser.users u
    left join ewuser.user_emails e
      on e.user_id = u.id
      and e.is_primary = true
    where u.id = $1 :: uuid
    limit 1
  |]

-- | Set user status.
-- Input: (user_id, status_text)
adminSetUserStatus :: Statement (UUID, Text) (Maybe (UUID, Maybe Text, Maybe Text, Text))
adminSetUserStatus =
  [TH.maybeStatement|
    with upd as (
      update ewuser.users
      set status = (($2 :: text)::ewuser.user_status)
      where id = $1 :: uuid
      returning id :: uuid
    )
    select
      u.id :: uuid,
      (e.email :: text) :: text?,
      u.display_name :: text?,
      u.status :: text
    from upd
    join ewuser.users u on u.id = upd.id
    left join ewuser.user_emails e
      on e.user_id = u.id
      and e.is_primary = true
    limit 1
  |]


-- ==========================================================================
-- Admin: wapp memberships
-- ==========================================================================

-- | List members of a wapp.
-- Returns (user_id, role_name, added_at_text)
adminListWappMembers :: Statement UUID (Vector (UUID, Text, Text))
adminListWappMembers =
  [TH.vectorStatement|
    select
      wm.user_id :: uuid,
      r.name :: text,
      to_char(wm.created_at, 'YYYY-MM-DD"T"HH24:MI:SSOF') :: text
    from ewuser.wapp_memberships wm
    join ewuser.roles r on r.id = wm.role_id
    where wm.wapp_id = $1 :: uuid
      and wm.status = 'active'
    order by wm.created_at asc
  |]

-- | Upsert a wapp member (add or replace role by name).
-- Input: (wapp_id, user_id, role_name)
-- Output: (user_id, role_name, added_at_text)
adminUpsertWappMemberRole :: Statement (UUID, UUID, Text) (Maybe (UUID, Text, Text))
adminUpsertWappMemberRole =
  [TH.maybeStatement|
    with role_row as (
      select r.id :: uuid, r.name :: text
      from ewuser.roles r
      where r.scope = 'wapp'
        and r.name = $3 :: text
      limit 1
    ),
    up as (
      insert into ewuser.wapp_memberships (wapp_id, user_id, role_id, status)
      select $1 :: uuid, $2 :: uuid, role_row.id :: uuid, 'active'
      from role_row
      on conflict (wapp_id, user_id)
      do update set
        role_id = excluded.role_id,
        status = 'active'
      returning user_id :: uuid, role_id :: uuid, created_at :: timestamptz
    )
    select
      up.user_id :: uuid,
      rr.name :: text,
      to_char(up.created_at, 'YYYY-MM-DD"T"HH24:MI:SSOF') :: text
    from up
    join role_row rr on rr.id = up.role_id
  |]

-- | Remove a member from a wapp (hard delete).
-- Input: (wapp_id, user_id)
-- Output: True if removed.
adminRemoveWappMember :: Statement (UUID, UUID) Bool
adminRemoveWappMember =
  [TH.singletonStatement|
    with x as (
      delete from ewuser.wapp_memberships
      where wapp_id = $1 :: uuid
        and user_id = $2 :: uuid
      returning 1 :: int4
    )
    select coalesce((select true from x limit 1), false) :: bool
  |]


-- ==========================================================================
-- OAuth flow state (PKCE / nonce)
-- ==========================================================================

-- | Insert an OAuth/OIDC flow state.
-- Input: (state, provider_id, code_verifier_hash, nonce?, redirect_to?, expires_at)
insertOAuthFlow :: Statement (Text, UUID, Bs.ByteString, Maybe Text, Maybe Text, Text) ()
insertOAuthFlow =
  [TH.resultlessStatement|
    insert into ewuser.oauth_flows (
      state,
      provider_id,
      code_verifier_hash,
      nonce,
      redirect_to,
      expires_at
    )
    values (
      $1 :: text,
      $2 :: uuid,
      $3 :: bytea,
      $4 :: text?,
      $5 :: text?,
      ($6 :: text)::timestamptz
    )
    on conflict (state)
    do update set
      provider_id = excluded.provider_id,
      code_verifier_hash = excluded.code_verifier_hash,
      nonce = excluded.nonce,
      redirect_to = excluded.redirect_to,
      expires_at = excluded.expires_at
  |]

-- NOTE: ByteString import intentionally omitted above because project conventions vary.
-- If you use strict ByteString: add `import Data.ByteString (ByteString)`.

-- | Read and delete (consume) an OAuth flow by state.
-- Returns:
--   (provider_id, code_verifier_hash, nonce?, redirect_to?) if not expired.
consumeOAuthFlow :: Statement Text (Maybe (UUID, Bs.ByteString, Maybe Text, Maybe Text))
consumeOAuthFlow =
  [TH.maybeStatement|
    with row as (
      select
        provider_id :: uuid,
        code_verifier_hash :: bytea,
        nonce :: text?,
        redirect_to :: text?
      from ewuser.oauth_flows
      where state = $1 :: text
        and expires_at > now()
      limit 1
    ),
    del as (
      delete from ewuser.oauth_flows
      where state = $1 :: text
    )
    select
      row.provider_id :: uuid,
      row.code_verifier_hash :: bytea,
      row.nonce :: text?,
      row.redirect_to :: text?
    from row
  |]


-- ==========================================================================
-- Auth events (audit)
-- ==========================================================================

-- | Append an auth event.
-- Input: (user_id?, session_id?, wapp_id?, kind_text, ip_text?, user_agent?, detail_jsonb)
insertAuthEvent :: Statement (Maybe UUID, Maybe UUID, Maybe UUID, Text, Maybe Text, Maybe Text, Value) ()
insertAuthEvent =
  [TH.resultlessStatement|
    insert into ewuser.auth_events (
      user_id,
      session_id,
      wapp_id,
      kind,
      ip_addr,
      user_agent,
      detail
    )
    values (
      $1 :: uuid?,
      $2 :: uuid?,
      $3 :: uuid?,
      (($4 :: text)::ewuser.auth_event_kind),
      (case when $5 :: text? is null then null else (($5 :: text?)::inet) end),
      $6 :: text?,
      $7 :: jsonb
    )
  |]
