module Wapp.UI.Auth where

import Network.URI.Encode (encode)

signupPanel :: String -> String -> String -> String
signupPanel appID apiServer nextStep = 
  let
    apiServerEncoded = encode apiServer
  in
  "<div class=\"ew-auth\" data-panel=\"signup\">\
\  <style>\
\    .ew-auth { color: #e5e7eb; }\
\    .ew-auth * { box-sizing: border-box; }\
\    .ew-auth .card { border: 1px solid rgba(255,255,255,0.10); background: rgba(255,255,255,0.03); border-radius: 14px; padding: 14px; }\
\    .ew-auth .top { display:flex; justify-content:space-between; align-items:flex-start; gap:12px; margin-bottom:12px; }\
\    .ew-auth .title { font-weight:780; letter-spacing:0.2px; font-size:16px; margin:0; line-height:1.1; }\
\    .ew-auth .subtitle { margin-top:6px; font-size:12px; color: rgba(229,231,235,0.68); line-height:1.35; }\
\    .ew-auth .tabs { display:inline-flex; border: 1px solid rgba(255,255,255,0.10); background: rgba(255,255,255,0.04); border-radius: 12px; overflow:hidden; }\
\    .ew-auth .tab { appearance:none; border:0; background:transparent; color: rgba(229,231,235,0.80); padding: 8px 10px; cursor:pointer; font-weight:700; font-size:12px; letter-spacing:0.2px; }\
\    .ew-auth .tab.active { background: rgba(96,165,250,0.18); color: rgba(229,231,235,0.98); }\
\    .ew-auth .grid { display:grid; gap:10px; }\
\    .ew-auth label { display:block; font-size:12px; font-weight:650; color: rgba(229,231,235,0.88); margin: 0 0 6px; }\
\    .ew-auth input { width:100%; padding:10px 12px; border-radius:12px; border: 1px solid rgba(255,255,255,0.10); background: rgba(0,0,0,0.20); color:#e5e7eb; outline:none; font-size:13px; }\
\    .ew-auth input:focus { border-color: rgba(96,165,250,0.45); box-shadow: 0 0 0 3px rgba(96,165,250,0.18); }\
\    .ew-auth .btn { appearance:none; width:100%; border:1px solid rgba(255,255,255,0.12); background: rgba(255,255,255,0.06); color:#e5e7eb; border-radius:12px; padding:10px 12px; cursor:pointer; font-weight:750; font-size:13px; }\
\    .ew-auth .btn.primary { background: rgba(96,165,250,0.20); border-color: rgba(96,165,250,0.38); }\
\    .ew-auth .btn:active { transform: translateY(1px); }\
\    .ew-auth .divider { display:grid; grid-template-columns:1fr auto 1fr; align-items:center; gap:10px; margin:12px 0; color: rgba(229,231,235,0.55); font-size:12px; }\
\    .ew-auth .divider:before, .ew-auth .divider:after { content:\"\"; height:1px; background: rgba(255,255,255,0.10); }\
\    .ew-auth .oauthGrid { display:grid; gap:10px; }\
\    .ew-auth .oauthBtn { display:inline-flex; align-items:center; justify-content:center; gap:10px; text-decoration:none; user-select:none; }\
\    .ew-auth .oauthBtn svg { width:16px; height:16px; }\
\    .ew-auth .foot { margin-top:10px; font-size:12px; color: rgba(229,231,235,0.60); line-height:1.35; }\
\    .ew-auth .link { color: rgba(96,165,250,0.92); text-decoration:none; font-weight:650; font-size:12px; }\
\    .ew-auth .link:hover { text-decoration: underline; }\
\    .ew-auth .alert { border: 1px solid rgba(248,113,113,0.35); background: rgba(248,113,113,0.10); color: rgba(254,226,226,0.96); padding: 10px 12px; border-radius: 12px; font-size: 12px; }\
\    .ew-auth .fineprint { margin-top: 10px; font-size: 11px; color: rgba(229,231,235,0.55); line-height: 1.35; }\
\  </style>\
\  <div class=\"card\">\
\    <div class=\"top\">\
\      <div>\
\        <div class=\"title\">Create your account</div>\
\        <div class=\"subtitle\">Start with email, or use a trusted provider.</div>\
\      </div>\
\    </div>\
\    <!-- Optional server-rendered errors container -->\
\    <!-- <div class=\"alert\">That email is already in use.</div> -->\
\    <form\
\      class=\"grid\"\
\      hx-post=\"" <> apiServer <> "/wbap/auth/signup?ewh=" <> apiServerEncoded <> "\"\
\      hx-target=\"#auth_modal_content\"\
\      hx-swap=\"innerHTML\"\
\      hx-indicator=\"#auth_modal_indicator\"\
\    >\
\      <input type=\"hidden\" name=\"app\" value=\"" <> appID <> "\" />\
\      <input type=\"hidden\" name=\"nextStep\" value=\"" <> nextStep <> "\" />\
\      <div>\
\        <label for=\"ew_signup_email\">Email</label>\
\        <input\
\          id=\"ew_signup_email\"\
\          name=\"email\"\
\          type=\"email\"\
\          inputmode=\"email\"\
\          autocomplete=\"email\"\
\          required\
\          placeholder=\"you@company.com\"\
\        />\
\      </div>\
\      <div>\
\        <label for=\"ew_signup_password\">Password</label>\
\        <input\
\          id=\"ew_signup_password\"\
\          name=\"password\"\
\          type=\"password\"\
\          autocomplete=\"new-password\"\
\          required\
\          placeholder=\"Create a password\"\
\        />\
\      </div>\
\      <div>\
\        <label for=\"ew_signup_password2\">Confirm password</label>\
\        <input\
\          id=\"ew_signup_password2\"\
\          name=\"password2\"\
\          type=\"password\"\
\          autocomplete=\"new-password\"\
\          required\
\          placeholder=\"Repeat your password\"\
\        />\
\      </div>\
\      <button class=\"btn primary\" type=\"submit\">Create account</button>\
\      <div class=\"divider\">or continue with</div>\
\      <div class=\"oauthGrid\">\
\        <a class=\"btn oauthBtn\" href=\"/wbap/auth/oidc/start?provider=google\">\
\          <svg viewBox=\"0 0 24 24\" fill=\"none\" aria-hidden=\"true\">\
\            <path d=\"M21.8 12.3c0-.7-.1-1.2-.2-1.8H12v3.4h5.5c-.1.9-.7 2.3-2 3.2v2.2h3.3c2-1.8 3-4.5 3-7.2Z\" fill=\"currentColor\" opacity=\".85\"/>\
\            <path d=\"M12 22c2.7 0 5-0.9 6.7-2.5l-3.3-2.2c-.9.6-2.1 1.1-3.4 1.1-2.6 0-4.8-1.7-5.6-4.1H3v2.3C4.7 19.9 8.1 22 12 22Z\" fill=\"currentColor\" opacity=\".75\"/>\
\            <path d=\"M6.4 14.3c-.2-.6-.3-1.2-.3-1.8s.1-1.2.3-1.8V8.4H3C2.4 9.6 2 11 2 12.5s.4 2.9 1 4.1l3.4-2.3Z\" fill=\"currentColor\" opacity=\".6\"/>\
\            <path d=\"M12 6.1c1.5 0 2.6.6 3.2 1.2l2.3-2.2C17 3.7 14.7 3 12 3 8.1 3 4.7 5.1 3 8.4l3.4 2.3C7.2 7.8 9.4 6.1 12 6.1Z\" fill=\"currentColor\" opacity=\".9\"/>\
\          </svg>\
\          Continue with Google\
\        </a>\
\        <a class=\"btn oauthBtn\" href=\"/wbap/auth/oidc/start?provider=microsoft\">\
\          <svg viewBox=\"0 0 24 24\" fill=\"none\" aria-hidden=\"true\">\
\            <path d=\"M3 3h8v8H3V3Z\" fill=\"currentColor\" opacity=\".90\"/>\
\            <path d=\"M13 3h8v8h-8V3Z\" fill=\"currentColor\" opacity=\".65\"/>\
\            <path d=\"M3 13h8v8H3v-8Z\" fill=\"currentColor\" opacity=\".65\"/>\
\            <path d=\"M13 13h8v8h-8v-8Z\" fill=\"currentColor\" opacity=\".90\"/>\
\          </svg>\
\          Continue with Microsoft\
\        </a>\
\        <a class=\"btn oauthBtn\" href=\"/wbap/auth/oidc/start?provider=twitter\">\
\          <svg viewBox=\"0 0 24 24\" fill=\"none\" aria-hidden=\"true\">\
\            <path d=\"M18.9 2H22l-6.8 7.8L22.8 22H16l-5.3-6.6L4.9 22H2l7.4-8.5L1.2 2H8l4.8 6L18.9 2Zm-2 18h1.7L7.1 4H5.3l11.6 16Z\" fill=\"currentColor\" opacity=\".9\"/>\
\          </svg>\
\          Continue with Twitter\
\        </a>\
\      </div>\
\      <div class=\"fineprint\">\
\        By creating an account, you agree to the platform’s Terms and acknowledge the Privacy Policy.\
\      </div>\
\      <div class=\"foot\">\
\        Already have an account?\
\        <button\
\          type=\"button\"\
\          class=\"link\"\
\          style=\"border: 0; background: transparent; padding: 0; cursor: pointer;\"\
\          hx-get=\"" <> apiServer <> "/wbap/ui/auth/signin?app=" <> appID <> "&ewh=" <> apiServerEncoded <> "&nextStep=" <> nextStep <> "\"\
\          hx-target=\"#auth_modal_content\"\
\          hx-swap=\"innerHTML\"\
\          hx-indicator=\"#auth_modal_indicator\"\
\        >\
\          Sign in\
\        </button>\
\      </div>\
\    </form>\
\  </div>\
\</div>"


signinPanel :: String -> String -> String -> String
signinPanel appID apiServer nextStep = 
  let
    apiServerEncoded = encode apiServer
  in
  "<div class=\"ew-auth\" data-panel=\"signin\">\
\  <style>\
\    .ew-auth { color: #e5e7eb; }\
\    .ew-auth * { box-sizing: border-box; }\
\    .ew-auth .card {\
\      border: 1px solid rgba(255,255,255,0.10);\
\      background: rgba(255,255,255,0.03);\
\      border-radius: 14px;\
\      padding: 14px;\
\    }\
\    .ew-auth .top {\
\      display: flex;\
\      justify-content: space-between;\
\      align-items: flex-start;\
\      gap: 12px;\
\      margin-bottom: 12px;\
\    }\
\    .ew-auth .title {\
\      font-weight: 780;\
\      letter-spacing: 0.2px;\
\      font-size: 16px;\
\      margin: 0;\
\      line-height: 1.1;\
\    }\
\    .ew-auth .subtitle {\
\      margin-top: 6px;\
\      font-size: 12px;\
\      color: rgba(229,231,235,0.68);\
\      line-height: 1.35;\
\    }\
\    .ew-auth .tabs {\
\      display: inline-flex;\
\      border: 1px solid rgba(255,255,255,0.10);\
\      background: rgba(255,255,255,0.04);\
\      border-radius: 12px;\
\      overflow: hidden;\
\    }\
\    .ew-auth .tab {\
\      appearance: none;\
\      border: 0;\
\      background: transparent;\
\      color: rgba(229,231,235,0.80);\
\      padding: 8px 10px;\
\      cursor: pointer;\
\      font-weight: 700;\
\      font-size: 12px;\
\      letter-spacing: 0.2px;\
\    }\
\    .ew-auth .tab.active {\
\      background: rgba(96,165,250,0.18);\
\      color: rgba(229,231,235,0.98);\
\    }\
\    .ew-auth .grid { display: grid; gap: 10px; }\
\    .ew-auth label {\
\      display: block;\
\      font-size: 12px;\
\      font-weight: 650;\
\      color: rgba(229,231,235,0.88);\
\      margin: 0 0 6px;\
\    }\
\    .ew-auth input {\
\      width: 100%;\
\      padding: 10px 12px;\
\      border-radius: 12px;\
\      border: 1px solid rgba(255,255,255,0.10);\
\      background: rgba(0,0,0,0.20);\
\      color: #e5e7eb;\
\      outline: none;\
\      font-size: 13px;\
\    }\
\    .ew-auth input:focus {\
\      border-color: rgba(96,165,250,0.45);\
\      box-shadow: 0 0 0 3px rgba(96,165,250,0.18);\
\    }\
\    .ew-auth .row {\
\      display: flex;\
\      align-items: center;\
\      justify-content: space-between;\
\      gap: 10px;\
\    }\
\    .ew-auth .link {\
\      color: rgba(96,165,250,0.92);\
\      text-decoration: none;\
\      font-weight: 650;\
\      font-size: 12px;\
\    }\
\    .ew-auth .link:hover { text-decoration: underline; }\
\    .ew-auth .btn {\
\      appearance: none;\
\      width: 100%;\
\      border: 1px solid rgba(255,255,255,0.12);\
\      background: rgba(255,255,255,0.06);\
\      color: #e5e7eb;\
\      border-radius: 12px;\
\      padding: 10px 12px;\
\      cursor: pointer;\
\      font-weight: 750;\
\      font-size: 13px;\
\    }\
\    .ew-auth .btn.primary {\
\      background: rgba(96,165,250,0.20);\
\      border-color: rgba(96,165,250,0.38);\
\    }\
\    .ew-auth .btn:active { transform: translateY(1px); }\
\    .ew-auth .divider {\
\      display: grid;\
\      grid-template-columns: 1fr auto 1fr;\
\      align-items: center;\
\      gap: 10px;\
\      margin: 12px 0;\
\      color: rgba(229,231,235,0.55);\
\      font-size: 12px;\
\    }\
\    .ew-auth .divider:before,\
\    .ew-auth .divider:after {\
\      content: \"\";\
\      height: 1px;\
\      background: rgba(255,255,255,0.10);\
\    }\
\    .ew-auth .oauthGrid { display: grid; gap: 10px; }\
\    .ew-auth .oauthBtn {\
\      display: inline-flex;\
\      align-items: center;\
\      justify-content: center;\
\      gap: 10px;\
\      text-decoration: none;\
\      user-select: none;\
\    }\
\    .ew-auth .oauthBtn svg { width: 16px; height: 16px; }\
\    .ew-auth .foot {\
\      margin-top: 10px;\
\      font-size: 12px;\
\      color: rgba(229,231,235,0.60);\
\      line-height: 1.35;\
\    }\
\    .ew-auth .alert {\
\      border: 1px solid rgba(248,113,113,0.35);\
\      background: rgba(248,113,113,0.10);\
\      color: rgba(254,226,226,0.96);\
\      padding: 10px 12px;\
\      border-radius: 12px;\
\      font-size: 12px;\
\    }\
\  </style>\
\  <div class=\"card\">\
\    <div class=\"top\">\
\      <div>\
\        <div class=\"title\">Welcome back</div>\
\        <div class=\"subtitle\">Sign in to continue to your EasyWordy workspace.</div>\
\      </div>\
\    </div>\
\    <!-- Optional server-rendered errors container (replace / omit as needed) -->\
\    <!-- <div class=\"alert\">Invalid credentials. Please try again.</div> -->\
\    <form\
\      class=\"grid\"\
\      hx-post=\"" <> apiServer <> "/wbap/auth/signin?ewh=" <> apiServerEncoded <> "\"\
\      hx-target=\"#auth_modal_content\"\
\      hx-swap=\"innerHTML\"\
\      hx-indicator=\"#auth_modal_indicator\"\
\    >\
\      <input type=\"hidden\" name=\"wappID\" value=\"" <> appID <> "\" />\
\      <input type=\"hidden\" name=\"nextStep\" value=\"" <> nextStep <> "\" />\
\      <div>\
\        <label for=\"ew_signin_email\">Email</label>\
\        <input\
\          id=\"ew_signin_email\"\
\          name=\"email\"\
\          type=\"email\"\
\          inputmode=\"email\"\
\          autocomplete=\"email\"\
\          required\
\          placeholder=\"you@company.com\"\
\        />\
\      </div>\
\      <div>\
\        <div class=\"row\" style=\"margin-bottom: 6px;\">\
\          <label for=\"ew_signin_password\" style=\"margin: 0;\">Password</label>\
\          <a\
\            class=\"link\"\
\            href=\"/wbap/ui/password_reset\"\
\            hx-get=\"" <> apiServer <> "/wbap/ui/auth/password_reset?app=" <> appID <> "&ewh=" <> apiServerEncoded <> "\"\
\            hx-target=\"#auth_modal_content\"\
\            hx-swap=\"innerHTML\"\
\            hx-indicator=\"#auth_modal_indicator\"\
\          >\
\            Forgot?\
\          </a>\
\        </div>\
\        <input\
\          id=\"ew_signin_password\"\
\          name=\"password\"\
\          type=\"password\"\
\          autocomplete=\"current-password\"\
\          required\
\          placeholder=\"Your password\"\
\        />\
\      </div>\
\      <button class=\"btn primary\" type=\"submit\">Sign in</button>\
\      <div class=\"divider\">or continue with</div>\
\      <div class=\"oauthGrid\">\
\        <!-- OIDC: use normal navigation for redirects (recommended) -->\
\        <a class=\"btn oauthBtn\" href=\"/wbap/auth/oidc/start?provider=google\">\
\          <!-- Google icon (simple) -->\
\          <svg viewBox=\"0 0 24 24\" fill=\"none\" aria-hidden=\"true\">\
\            <path d=\"M21.8 12.3c0-.7-.1-1.2-.2-1.8H12v3.4h5.5c-.1.9-.7 2.3-2 3.2v2.2h3.3c2-1.8 3-4.5 3-7.2Z\" fill=\"currentColor\" opacity=\".85\"/>\
\            <path d=\"M12 22c2.7 0 5-0.9 6.7-2.5l-3.3-2.2c-.9.6-2.1 1.1-3.4 1.1-2.6 0-4.8-1.7-5.6-4.1H3v2.3C4.7 19.9 8.1 22 12 22Z\" fill=\"currentColor\" opacity=\".75\"/>\
\            <path d=\"M6.4 14.3c-.2-.6-.3-1.2-.3-1.8s.1-1.2.3-1.8V8.4H3C2.4 9.6 2 11 2 12.5s.4 2.9 1 4.1l3.4-2.3Z\" fill=\"currentColor\" opacity=\".6\"/>\
\            <path d=\"M12 6.1c1.5 0 2.6.6 3.2 1.2l2.3-2.2C17 3.7 14.7 3 12 3 8.1 3 4.7 5.1 3 8.4l3.4 2.3C7.2 7.8 9.4 6.1 12 6.1Z\" fill=\"currentColor\" opacity=\".9\"/>\
\          </svg>\
\          Continue with Google\
\        </a>\
\        <a class=\"btn oauthBtn\" href=\"/wbap/auth/oidc/start?provider=microsoft\">\
\          <!-- Microsoft icon -->\
\          <svg viewBox=\"0 0 24 24\" fill=\"none\" aria-hidden=\"true\">\
\            <path d=\"M3 3h8v8H3V3Z\" fill=\"currentColor\" opacity=\".90\"/>\
\            <path d=\"M13 3h8v8h-8V3Z\" fill=\"currentColor\" opacity=\".65\"/>\
\            <path d=\"M3 13h8v8H3v-8Z\" fill=\"currentColor\" opacity=\".65\"/>\
\            <path d=\"M13 13h8v8h-8v-8Z\" fill=\"currentColor\" opacity=\".90\"/>\
\          </svg>\
\          Continue with Microsoft\
\        </a>\
\        <a class=\"btn oauthBtn\" href=\"/wbap/auth/oidc/start?provider=twitter\">\
\          <!-- X/Twitter icon (simple) -->\
\          <svg viewBox=\"0 0 24 24\" fill=\"none\" aria-hidden=\"true\">\
\            <path d=\"M18.9 2H22l-6.8 7.8L22.8 22H16l-5.3-6.6L4.9 22H2l7.4-8.5L1.2 2H8l4.8 6L18.9 2Zm-2 18h1.7L7.1 4H5.3l11.6 16Z\" fill=\"currentColor\" opacity=\".9\"/>\
\          </svg>\
\          Continue with Twitter\
\        </a>\
\      </div>\
\      <div class=\"foot\">\
\        New here?\
\        <button\
\          type=\"button\"\
\          class=\"link\"\
\          style=\"border: 0; background: transparent; padding: 0; cursor: pointer;\"\
\          hx-get=\"" <> apiServer <> "/wbap/ui/auth/signup?app=" <> appID <> "&ewh=" <> apiServerEncoded <> "\"\
\          hx-target=\"#auth_modal_content\"\
\          hx-swap=\"innerHTML\"\
\          hx-indicator=\"#auth_modal_indicator\"\
\        >\
\          Create an account\
\        </button>\
\      </div>\
\    </form>\
\  </div>\
\</div>"