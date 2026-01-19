-- user_schema_v1.sql
-- EasyWordy: user management + authentication + authorization (RBAC) + session/context model
--
-- Notes:
--   * The schema name is ewuser (quoted because USER is a keyword).
--   * Sessions represent client connections; contexts represent a persisted Wapp interaction state.
--   * Multiple sessions may attach to the same context (session renewal / multi-device).
--   * External identities support OIDC/OAuth providers and brokers (Clerk/WorkOS) via provider+subject mapping.

BEGIN;

-- -----------------------------------------------------------------------------
-- Extensions
-- -----------------------------------------------------------------------------

CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS citext;

-- -----------------------------------------------------------------------------
-- Schema
-- -----------------------------------------------------------------------------

CREATE SCHEMA IF NOT EXISTS ewuser;

SET search_path = ewuser, public;

-- -----------------------------------------------------------------------------
-- Types (enums)
-- -----------------------------------------------------------------------------

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_type t
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE n.nspname = 'user' AND t.typname = 'user_status'
  ) THEN
    CREATE TYPE ewuser.user_status AS ENUM (
      'active',
      'disabled',
      'deleted'
    );
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM pg_type t
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE n.nspname = 'user' AND t.typname = 'provider_kind'
  ) THEN
    CREATE TYPE ewuser.provider_kind AS ENUM (
      'oidc',          -- native OIDC (Google/Microsoft, etc.)
      'oauth2',        -- OAuth2 without standard id_token
      'broker_oidc',   -- broker yields OIDC-like identity (Clerk/WorkOS)
      'saml_broker'    -- broker handles SAML and yields OIDC/JWT outcome
    );
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM pg_type t
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE n.nspname = 'user' AND t.typname = 'session_kind'
  ) THEN
    CREATE TYPE ewuser.session_kind AS ENUM (
      'browser',
      'native_client',
      'service_client'
    );
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM pg_type t
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE n.nspname = 'user' AND t.typname = 'role_scope'
  ) THEN
    CREATE TYPE ewuser.role_scope AS ENUM (
      'platform',
      'wapp'
    );
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM pg_type t
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE n.nspname = 'user' AND t.typname = 'auth_event_kind'
  ) THEN
    CREATE TYPE ewuser.auth_event_kind AS ENUM (
      'login_success',
      'login_failed',
      'logout',
      'session_renew',
      'password_set',
      'password_reset_request',
      'password_reset_complete',
      'email_verify_request',
      'email_verified',
      'provider_linked',
      'provider_unlinked',
      'session_revoked'
    );
  END IF;
END $$;

-- -----------------------------------------------------------------------------
-- Shared helpers
-- -----------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION ewuser.set_updated_at()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  NEW.updated_at := now();
  RETURN NEW;
END;
$$;

-- -----------------------------------------------------------------------------
-- Identity: users + profile + emails
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.users (
  id           uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  handle       text,
  display_name text,
  status       ewuser.user_status NOT NULL DEFAULT 'active',
  created_at   timestamptz NOT NULL DEFAULT now(),
  updated_at   timestamptz NOT NULL DEFAULT now()
);

CREATE TRIGGER trg_users_updated_at
BEFORE UPDATE ON ewuser.users
FOR EACH ROW EXECUTE FUNCTION ewuser.set_updated_at();

CREATE TABLE IF NOT EXISTS ewuser.user_emails (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_fk     uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  email       citext NOT NULL,
  is_primary  boolean NOT NULL DEFAULT false,
  is_verified boolean NOT NULL DEFAULT false,
  verified_at timestamptz,
  created_at  timestamptz NOT NULL DEFAULT now()
);

-- Email unique across the whole system (case-insensitive by citext)
CREATE UNIQUE INDEX IF NOT EXISTS ux_user_emails_email
ON ewuser.user_emails(email);

-- At most one primary email per user
CREATE UNIQUE INDEX IF NOT EXISTS ux_user_emails_primary_per_user
ON ewuser.user_emails(user_fk)
WHERE is_primary;

-- Helpful lookup
CREATE INDEX IF NOT EXISTS ix_user_emails_user_fk
ON ewuser.user_emails(user_fk);

CREATE TABLE IF NOT EXISTS ewuser.user_profile (
  user_fk    uuid PRIMARY KEY REFERENCES ewuser.users(id) ON DELETE CASCADE,
  avatar_url text,
  locale     text,
  timezone   text,
  meta       jsonb NOT NULL DEFAULT '{}'::jsonb,
  updated_at timestamptz NOT NULL DEFAULT now()
);

CREATE TRIGGER trg_user_profile_updated_at
BEFORE UPDATE ON ewuser.user_profile
FOR EACH ROW EXECUTE FUNCTION ewuser.set_updated_at();

-- -----------------------------------------------------------------------------
-- Credentials: password
-- -----------------------------------------------------------------------------

-- Store password hashes as PHC strings (Argon2id recommended)
CREATE TABLE IF NOT EXISTS ewuser.password_credentials (
  user_fk             uuid PRIMARY KEY REFERENCES ewuser.users(id) ON DELETE CASCADE,
  password_hash       text NOT NULL,
  password_updated_at timestamptz NOT NULL DEFAULT now(),
  must_change         boolean NOT NULL DEFAULT false
);

-- -----------------------------------------------------------------------------
-- External providers + identity links (OIDC/OAuth/Brokers)
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.identity_providers (
  id                uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name              text NOT NULL, -- e.g. "google", "microsoft", "clerk", "workos"
  kind              ewuser.provider_kind NOT NULL,
  issuer            text,          -- OIDC issuer URL when relevant
  authorization_url text,
  token_url         text,
  jwks_url          text,
  client_fk         text,
  client_secret_enc bytea,         -- encrypted at rest by application
  config            jsonb NOT NULL DEFAULT '{}'::jsonb,
  is_enabled        boolean NOT NULL DEFAULT true,
  created_at        timestamptz NOT NULL DEFAULT now(),
  updated_at        timestamptz NOT NULL DEFAULT now(),
  UNIQUE(name)
);

CREATE TRIGGER trg_fkentity_providers_updated_at
BEFORE UPDATE ON ewuser.identity_providers
FOR EACH ROW EXECUTE FUNCTION ewuser.set_updated_at();

-- provider+subject is the stable key (OIDC sub, broker subject, etc.)
CREATE TABLE IF NOT EXISTS ewuser.external_fkentities (
  id            uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_fk       uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  provider_fk   uuid NOT NULL REFERENCES ewuser.identity_providers(id) ON DELETE RESTRICT,
  subject       text NOT NULL,
  email_hint    citext,
  created_at    timestamptz NOT NULL DEFAULT now(),
  last_seen_at  timestamptz,
  UNIQUE(provider_fk, subject)
);

CREATE INDEX IF NOT EXISTS ix_external_fkentities_user_fk
ON ewuser.external_fkentities(user_fk);

-- -----------------------------------------------------------------------------
-- OAuth/OIDC flow state (PKCE / nonce / replay protection)
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.oauth_flows (
  state              text PRIMARY KEY,
  provider_fk         uuid NOT NULL REFERENCES ewuser.identity_providers(id) ON DELETE CASCADE,
  code_verifier_hash  bytea,     -- hash(verifier), so verifier itself isn't stored
  nonce               text,
  redirect_to         text,
  created_at          timestamptz NOT NULL DEFAULT now(),
  expires_at          timestamptz NOT NULL
);

CREATE INDEX IF NOT EXISTS ix_oauth_flows_expires_at
ON ewuser.oauth_flows(expires_at);

-- -----------------------------------------------------------------------------
-- Contexts: persisted Wapp interaction state
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.contexts (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_fk     uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  wapp_fk     uuid NOT NULL,
  status      text NOT NULL DEFAULT 'active',
  title       text,
  state_json  jsonb NOT NULL DEFAULT '{}'::jsonb,
  version     bigint NOT NULL DEFAULT 0,
  created_at  timestamptz NOT NULL DEFAULT now(),
  updated_at  timestamptz NOT NULL DEFAULT now()
);

CREATE TRIGGER trg_contexts_updated_at
BEFORE UPDATE ON ewuser.contexts
FOR EACH ROW EXECUTE FUNCTION ewuser.set_updated_at();

CREATE INDEX IF NOT EXISTS ix_contexts_user_wapp
ON ewuser.contexts(user_fk, wapp_fk);

-- -----------------------------------------------------------------------------
-- Sessions: represent client connections; attached to contexts
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.sessions (
  id           uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_fk      uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  context_fk   uuid REFERENCES ewuser.contexts(id) ON DELETE SET NULL,
  kind         ewuser.session_kind NOT NULL DEFAULT 'browser',
  created_at   timestamptz NOT NULL DEFAULT now(),
  last_seen_at timestamptz NOT NULL DEFAULT now(),
  revoked_at   timestamptz,
  ip_addr      inet,
  user_agent   text,
  meta         jsonb NOT NULL DEFAULT '{}'::jsonb
);

CREATE INDEX IF NOT EXISTS ix_sessions_user_fk
ON ewuser.sessions(user_fk);

CREATE INDEX IF NOT EXISTS ix_sessions_context_fk
ON ewuser.sessions(context_fk);

CREATE INDEX IF NOT EXISTS ix_sessions_last_seen
ON ewuser.sessions(last_seen_at);

CREATE INDEX IF NOT EXISTS ix_sessions_active
ON ewuser.sessions(user_fk)
WHERE revoked_at IS NULL;

-- -----------------------------------------------------------------------------
-- Refresh tokens (optional now; supports rotation & revocation)
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.refresh_tokens (
  id            uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  session_fk    uuid NOT NULL REFERENCES ewuser.sessions(id) ON DELETE CASCADE,
  token_hash    bytea NOT NULL, -- e.g. SHA-256 of opaque refresh token
  created_at    timestamptz NOT NULL DEFAULT now(),
  expires_at    timestamptz NOT NULL,
  revoked_at    timestamptz,
  rotated_from  uuid REFERENCES ewuser.refresh_tokens(id) ON DELETE SET NULL,
  UNIQUE(session_fk, token_hash)
);

CREATE INDEX IF NOT EXISTS ix_refresh_tokens_session
ON ewuser.refresh_tokens(session_fk);

CREATE INDEX IF NOT EXISTS ix_refresh_tokens_expires
ON ewuser.refresh_tokens(expires_at);

-- -----------------------------------------------------------------------------
-- Authorization: permissions, roles, role-permissions
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.permissions (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name        text NOT NULL,
  description text,
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE(name)
);

CREATE TABLE IF NOT EXISTS ewuser.roles (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  scope       ewuser.role_scope NOT NULL,
  name        text NOT NULL,
  description text,
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE(scope, name)
);

CREATE TABLE IF NOT EXISTS ewuser.role_permissions (
  role_fk       uuid NOT NULL REFERENCES ewuser.roles(id) ON DELETE CASCADE,
  permission_fk uuid NOT NULL REFERENCES ewuser.permissions(id) ON DELETE CASCADE,
  PRIMARY KEY (role_fk, permission_fk)
);

CREATE INDEX IF NOT EXISTS ix_role_permissions_permission
ON ewuser.role_permissions(permission_fk);

-- Platform role assignments (e.g. super_admin)
CREATE TABLE IF NOT EXISTS ewuser.user_roles (
  user_fk    uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  role_fk    uuid NOT NULL REFERENCES ewuser.roles(id) ON DELETE RESTRICT,
  granted_at timestamptz NOT NULL DEFAULT now(),
  granted_by uuid REFERENCES ewuser.users(id) ON DELETE SET NULL,
  PRIMARY KEY (user_fk, role_fk)
);

-- Wapp membership: binds a user to a role within a given wapp
CREATE TABLE IF NOT EXISTS ewuser.wapp_memberships (
  wapp_fk     uuid NOT NULL,
  user_fk     uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  role_fk     uuid NOT NULL REFERENCES ewuser.roles(id) ON DELETE RESTRICT,
  status      text NOT NULL DEFAULT 'active',
  created_at  timestamptz NOT NULL DEFAULT now(),
  created_by  uuid REFERENCES ewuser.users(id) ON DELETE SET NULL,
  PRIMARY KEY (wapp_fk, user_fk)
);

CREATE INDEX IF NOT EXISTS ix_wapp_memberships_user
ON ewuser.wapp_memberships(user_fk);

CREATE INDEX IF NOT EXISTS ix_wapp_memberships_role
ON ewuser.wapp_memberships(role_fk);

-- Optional grouping (teams) within a wapp; useful for governance / bulk role grants
CREATE TABLE IF NOT EXISTS ewuser.groups (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  wapp_fk     uuid,
  name        text NOT NULL,
  description text,
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE(wapp_fk, name)
);

CREATE TABLE IF NOT EXISTS ewuser.group_members (
  group_fk   uuid NOT NULL REFERENCES ewuser.groups(id) ON DELETE CASCADE,
  user_fk    uuid NOT NULL REFERENCES ewuser.users(id) ON DELETE CASCADE,
  added_at   timestamptz NOT NULL DEFAULT now(),
  added_by   uuid REFERENCES ewuser.users(id) ON DELETE SET NULL,
  PRIMARY KEY (group_fk, user_fk)
);

-- Groups can be bound to roles (platform or wapp scope).
CREATE TABLE IF NOT EXISTS ewuser.group_roles (
  group_fk  uuid NOT NULL REFERENCES ewuser.groups(id) ON DELETE CASCADE,
  role_fk   uuid NOT NULL REFERENCES ewuser.roles(id) ON DELETE RESTRICT,
  PRIMARY KEY (group_fk, role_fk)
);

-- Optional fine-grained resource grants (leave empty until needed)
CREATE TABLE IF NOT EXISTS ewuser.resource_grants (
  id            uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  wapp_fk        uuid,
  resource_type  text NOT NULL,
  resource_fk    uuid NOT NULL,
  subject_kind   text NOT NULL CHECK (subject_kind IN ('user', 'group')),
  subject_fk     uuid NOT NULL,
  permission_fk  uuid NOT NULL REFERENCES ewuser.permissions(id) ON DELETE CASCADE,
  created_at     timestamptz NOT NULL DEFAULT now(),
  created_by     uuid REFERENCES ewuser.users(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS ix_resource_grants_lookup
ON ewuser.resource_grants(wapp_fk, resource_type, resource_fk);

-- -----------------------------------------------------------------------------
-- Auditing / telemetry
-- -----------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS ewuser.auth_events (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_fk     uuid REFERENCES ewuser.users(id) ON DELETE SET NULL,
  session_fk  uuid REFERENCES ewuser.sessions(id) ON DELETE SET NULL,
  wapp_fk     uuid,
  kind        ewuser.auth_event_kind NOT NULL,
  at          timestamptz NOT NULL DEFAULT now(),
  ip_addr     inet,
  user_agent  text,
  detail      jsonb NOT NULL DEFAULT '{}'::jsonb
);

CREATE INDEX IF NOT EXISTS ix_auth_events_user_at
ON ewuser.auth_events(user_fk, at DESC);

CREATE INDEX IF NOT EXISTS ix_auth_events_kind_at
ON ewuser.auth_events(kind, at DESC);

-- -----------------------------------------------------------------------------
-- Bootstrap (optional): seed canonical roles/permissions (safe to re-run)
-- -----------------------------------------------------------------------------

-- Permissions (string convention; extend over time)
INSERT INTO ewuser.permissions (name, description)
VALUES
  ('platform.admin', 'Platform-wide administration'),
  ('wapp.admin.users', 'Manage wapp users and membership'),
  ('wapp.page.read', 'Read wapp pages/content'),
  ('wapp.page.edit', 'Edit wapp pages/content'),
  ('wapp.deploy', 'Deploy/publish a wapp')
ON CONFLICT (name) DO NOTHING;

-- Roles
INSERT INTO ewuser.roles (scope, name, description)
VALUES
  ('platform', 'super_admin', 'Full platform control'),
  ('platform', 'support_admin', 'Support operations'),
  ('wapp', 'owner', 'Full control of a wapp'),
  ('wapp', 'admin', 'Administer a wapp'),
  ('wapp', 'editor', 'Edit within a wapp'),
  ('wapp', 'viewer', 'View within a wapp')
ON CONFLICT (scope, name) DO NOTHING;

-- Role-permission links (minimal defaults)
-- super_admin -> all current permissions
INSERT INTO ewuser.role_permissions (role_fk, permission_fk)
SELECT r.id, p.id
FROM ewuser.roles r
JOIN ewuser.permissions p ON TRUE
WHERE r.scope = 'platform' AND r.name = 'super_admin'
ON CONFLICT DO NOTHING;

-- support_admin -> platform.admin (and maybe user management later)
INSERT INTO ewuser.role_permissions (role_fk, permission_fk)
SELECT r.id, p.id
FROM ewuser.roles r
JOIN ewuser.permissions p ON p.name IN ('platform.admin')
WHERE r.scope = 'platform' AND r.name = 'support_admin'
ON CONFLICT DO NOTHING;

-- wapp roles
INSERT INTO ewuser.role_permissions (role_fk, permission_fk)
SELECT r.id, p.id
FROM ewuser.roles r
JOIN ewuser.permissions p ON p.name IN ('wapp.admin.users', 'wapp.page.read', 'wapp.page.edit', 'wapp.deploy')
WHERE r.scope = 'wapp' AND r.name = 'owner'
ON CONFLICT DO NOTHING;

INSERT INTO ewuser.role_permissions (role_fk, permission_fk)
SELECT r.id, p.id
FROM ewuser.roles r
JOIN ewuser.permissions p ON p.name IN ('wapp.admin.users', 'wapp.page.read', 'wapp.page.edit')
WHERE r.scope = 'wapp' AND r.name = 'admin'
ON CONFLICT DO NOTHING;

INSERT INTO ewuser.role_permissions (role_fk, permission_fk)
SELECT r.id, p.id
FROM ewuser.roles r
JOIN ewuser.permissions p ON p.name IN ('wapp.page.read', 'wapp.page.edit')
WHERE r.scope = 'wapp' AND r.name = 'editor'
ON CONFLICT DO NOTHING;

INSERT INTO ewuser.role_permissions (role_fk, permission_fk)
SELECT r.id, p.id
FROM ewuser.roles r
JOIN ewuser.permissions p ON p.name IN ('wapp.page.read')
WHERE r.scope = 'wapp' AND r.name = 'viewer'
ON CONFLICT DO NOTHING;

COMMIT;
