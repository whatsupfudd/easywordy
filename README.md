# EasyWordy

**The Haskell application server for FUDD Wapps.**

EasyWordy hosts web applications that combine server-rendered HTML,
Fuddle/Elm application logic, native Haskell functions, and database-backed
operations. It connects browser interactions to server-side execution through
HTTP and WebSockets, returning HTML fragments for HTMX to insert into the page.

The project also develops an evolutionary path from WordPress/PHP applications
toward the FUDD application model. Its codebase includes an embedded PHP
interpreter, WordPress routes, and MySQL integration alongside the Wapp runtime.

**Status:** experimental development, package version `0.1.0.0`.

This README describes repository revision `69424d9` dated 9 July 2026.
Several subsystems contain working execution paths alongside unfinished
integration. In particular, authentication, shared runtime management, and
WordPress compatibility must not be treated as production-complete.

## What EasyWordy provides

EasyWordy supplies the common server infrastructure used by individual Wapps:

- HTTP routing through Servant, WAI, and Warp.
- Discovery and resolution of YAML Wapp definitions.
- WebSocket connections associated with a selected Wapp.
- Dispatch to HTML files, internal Haskell functions, and compiled Fuddle/Elm
  JavaScript.
- Calls from Fuddle/Elm logic into native Haskell functions.
- HTML responses and HTMX out-of-band updates.
- PostgreSQL connection pools, including optional Wapp-specific pools.
- Development-time file watching and JavaScript reload.
- Embedded PHP execution and experimental WordPress integration.
- Optional S3-compatible storage integration.

A Wapp provides its own application content, routes, compiled logic, and
optional native libraries. EasyWordy loads those resources and handles the
interaction between the browser and the application.

## EasyWordy in the FUDD ecosystem

The main components have different responsibilities:

| Component | Responsibility |
| --- | --- |
| EasyWordy | Hosts applications, dispatches requests, and supplies runtime resources. |
| Wapp | Defines one application's identity, content, callable actions, and dependencies. |
| Fuddle/Elm | Supplies typed application and presentation logic compiled to JavaScript. |
| Native Haskell library | Implements application-specific server operations, including database access. |
| HTMX and its WebSocket extension | Send browser interactions and apply returned HTML. |
| PostgreSQL | Stores application data accessed through Hasql. |
| PHP/WordPress integration | Supports experiments in retaining and progressively replacing existing PHP behavior. |

The wider FUDD design supports static initial pages with server-backed
interactivity. Those initial pages and assets can be built and hosted
separately; EasyWordy supplies the application execution endpoint.

The Fuddle compiler, application sources, native Wapp packages, and complete
deployment tooling are not all contained in this repository.

## How a Wapp interaction works

1. A browser loads the application's initial HTML and client-side assets.
2. It opens a WebSocket connection to EasyWordy using the Wapp's UUID.
3. A browser interaction sends an HTMX-style JSON message containing an action
   identifier and optional parameters.
4. EasyWordy resolves that identifier in the Wapp's routing table.
5. The selected route reads an HTML file, calls an internal Haskell function,
   or invokes compiled Fuddle/Elm logic.
6. Fuddle/Elm logic may request native Haskell operations and resume when their
   results arrive.
7. EasyWordy returns HTML, including a target wrapper or out-of-band swap
   attributes where applicable.
8. The browser applies the response to the relevant page region.

Application actions are identified by the `mid` value in the message headers.
They are not automatically exposed as individual REST URLs.

## Current implementation status

| Area | Status in this revision |
| --- | --- |
| HTTP server | Servant/WAI/Warp startup and route composition are present. |
| Wapp definitions | YAML loading, UUID lookup, and function resolution are implemented. |
| File-backed actions | Read HTML/content from the Wapp content directory. |
| Internal Haskell actions | Resolved through `Wapp.InternalLib`. |
| Fuddle/Elm execution | JavaScript module loading and port-based invocation are implemented. |
| Native libraries | Loader and registry integration use the companion `ew-general` package; integration is still evolving. |
| HTMX responses | Targeted HTML responses and several out-of-band reply forms are implemented. |
| Development reload | Watches files and can reload the first configured JavaScript library. |
| Authentication and accounts | UI, route types, SQL, and handler skeletons exist; end-to-end authentication is unfinished. |
| Shared Wapp cache and sessions | Types exist, but persistent shared state management is unfinished. |
| PHP/WordPress | Embedded interpreter and route handling are present; full compatibility is not established. |
| Uploads | Experimental handler with a hard-coded destination directory. |
| Automated tests | The test executable currently prints a placeholder message. |

## Building

### Prerequisites

The repository currently assumes a FUDD development workspace rather than an
isolated checkout.

You need:

- Git and Haskell Stack.
- A GHC installation compatible with the configured Stack resolver.
- A C toolchain and PHP development headers.
- A PHP shared library suitable for the embedded interpreter.
- The local Haskell dependencies referenced by `stack.yaml`.
- The JavaScript runtime required by the local `inline-js` packages.
- PostgreSQL and MySQL for the current server startup path.

`stack.yaml` selects LTS 22.44 and enables `system-ghc: true`.
It also enables shared-library builds. The executable is dynamically linked.

### Companion packages

The following paths are configured relative to the repository:

| Path | Purpose |
| --- | --- |
| `../Lib/inline-js/inline-js` | JavaScript execution integration. |
| `../Lib/inline-js/inline-js-core` | Supporting JavaScript runtime integration. |
| `../../../Haskell/OpenAI/LocalLibs/hs-connection` | Local connection package. |
| `../../../Haskell/Minio/minio-hs` | S3-compatible storage client. |
| `../Wapp/Lib/Natives/General` | `ew-general`, including native loader and registry support. |
| `../Lib/php` | PHP headers and supporting directories. |
| `../Lib/php/libs/x86_64-linux` | Configured PHP shared-library directory. |

Provide these dependencies or adjust `stack.yaml` to match your workspace.
A standalone clone does not include them.

### Build the executable

```bash
git clone https://github.com/whatsupfudd/easywordy.git
cd easywordy

# First provision the companion packages and PHP dependencies.
stack build

stack exec -- easywordy --help
```

`package.yaml` is the package definition used by Hpack.
`easywordy.cabal` is generated from it.

The repository's `prepare.sh` contains a developer-specific macOS path and an
old `easyverse` command alias. Adapt it before use; it is not a portable setup
script.

## Configuration

### Configuration file selection

EasyWordy chooses its configuration file in this order:

1. The `--config` / `-c` command-line argument.
2. The `EASYWORDY_CONFIG` environment variable.
3. `$HOME/.fudd/easywordy/config.yaml`.

Only filenames ending in `.yaml` are accepted by the current parser.

The CLI help text mentions a different default directory. The path above is
the one implemented by `Options.ConfFile`.

Configuration is loaded before command dispatch, including for the `version`
and `help` subcommands. The parser's `--help` option can be used without a
configuration file.

### Application directories

Set `EASYWORDY` to an absolute application-home directory:

```bash
export EASYWORDY="$HOME/EasyWordy"

mkdir -p "$EASYWORDY/Wapp/Defs"
mkdir -p "$EASYWORDY/Wapp/Apps"
mkdir -p "$EASYWORDY/Wordpress"
mkdir -p "$HOME/.fudd/easywordy"
```

When `EASYWORDY` is unset, the application home defaults to
`$HOME/EasyWordy`.

The resulting defaults are:

| Resource | Default location |
| --- | --- |
| Wapp definitions | `$EASYWORDY/Wapp/Defs` |
| Wapp content | `$EASYWORDY/Wapp/Apps` |
| WordPress files | `$EASYWORDY/Wordpress` |
| Native libraries | `$EASYWORDY/.fudd/easywordy/natives` |

Although the configuration schema accepts `wapp.waDef` and `wapp.waContent`,
their merge into runtime options is commented out in this revision. Use the
application-home layout above.

### Local development configuration

Create `$HOME/.fudd/easywordy/config.yaml`:

```yaml
server:
  host: "127.0.0.1"
  port: 8885

db:
  host: "127.0.0.1"
  port: 5432
  user: "easywordy"
  passwd: "replace-with-local-postgres-password"
  dbase: "easywordy"

wordpress:
  rootPath: "$EASYWORDY/Wordpress"
  db:
    host: "127.0.0.1"
    port: 3306
    user: "easywordy"
    passwd: "replace-with-local-mysql-password"
    dbase: "wordpress"

jwt:
  jEnabled: false

cors:
  oEnabled: true
  allowed:
    - "http://localhost:8885"
    - "http://127.0.0.1:8885"
```

Provision the database accounts and databases separately. This configuration
does not create them.

Important implementation details:

- `server.host` is a bind hostname or address, without `http://`.
  Set it explicitly: the built-in default includes a URL scheme.
- PHP and MySQL are initialized by the current server startup path even when
  the immediate objective is to run Wapps.
- `jwt.jEnabled: false` selects an ephemeral JWT key. It does not remove the
  authentication machinery or complete the unfinished authentication flow.
- Use `jwt.keyFile` to select an existing JWK file when a stable key is needed.
- Prefer `--config` over `EASYWORDY_CONFIG`: the latter is also reused as a
  directory when deriving the default JWK path.
- YAML fields `debug`, `server.cache`, `db.poolSize`, and `db.poolTimeOut`
  are parsed but not applied by the current options merge. Use `--debug`
  for the runtime debug value.
- Environment-variable expansion is implemented for selected path settings;
  do not assume arbitrary YAML strings support it.

Optional top-level settings include `nativesRoot` and `s3store`.
The exact accepted field names are defined in `src/Options/ConfFile.hs`.

## Running

After building and configuring the runtime:

```bash
stack exec -- easywordy \
  --config "$HOME/.fudd/easywordy/config.yaml" \
  server
```

To display the package version and embedded Git revision:

```bash
stack exec -- easywordy \
  --config "$HOME/.fudd/easywordy/config.yaml" \
  version
```

Available commands:

| Command | Purpose |
| --- | --- |
| `server` | Start the application server. |
| `version` | Print package version and Git revision. |
| `help` | Placeholder command; use `--help` for actual CLI usage. |
| `testjs` | Developer experiment containing a hard-coded JavaScript path. |

Global options are `--config` / `-c` and `--debug` / `-d`.

The default port is `8885`. The root URL belongs to the WordPress route group;
it is not a Wapp directory or application launcher.

## Defining a Wapp

Wapp definitions are YAML files in the definitions directory.

The loader selects `.yaml` files whose filenames begin with a letter.
Definitions are read at server startup and indexed by UUID.

Each definition contains:

| Field | Meaning |
| --- | --- |
| `uid` | Unique UUID identifying the Wapp. |
| `label` | Human-readable application name. |
| `locales` | Declared locale identifiers. |
| `rootPath` | Application content path resolved beneath the Wapp content directory. |
| `libs` | Library references expressed as single-entry mappings. |
| `functions` | Action identifiers and their implementation references. |
| `db` | Optional application-specific PostgreSQL connection settings. |

A function definition has an `id`, an `action`, and optional `args`.

Supported action forms:

```yaml
action:
  FileVerbatim: "fragments/welcome.html"
```

```yaml
action:
  Function:
    Application: "showWelcome"
```

`Template` is recognized by the parser but rejected as unimplemented during
resolution.

### A small file-backed example

Create `Wapp/Defs/hello.yaml` beneath the application home:

```yaml
uid: "b3ca2e20-0f7a-4bbb-969f-6b27db80f642"
label: "Hello Wapp"
locales:
  - "en"
rootPath: "hello"

libs:
  - main: ""

functions:
  - id: "welcome"
    action:
      FileVerbatim: "welcome.html"

db: null
```

Create `Wapp/Apps/hello/welcome.html`:

```html
<p>Hello from EasyWordy.</p>
```

The empty library identifier is deliberate: the current WebSocket initializer
uses it to skip JavaScript initialization. An empty `libs` list instead falls
back to attempting to load a library named `main`.

Restart EasyWordy after adding the definition.

This example exercises the file-backed action path. It does not create an
initial browser page or eliminate the server's PHP/database startup
requirements.

### Connecting to the Wapp

The WebSocket endpoint for the example is:

```text
ws://127.0.0.1:8885/wbap/stream/b3ca2e20-0f7a-4bbb-969f-6b27db80f642
```

Although the route declaration names the captured parameter `sid`, the handler
currently interprets it as the **Wapp UUID**, not an authenticated session ID.

Send a text message such as:

```json
{
  "HEADERS": {
    "HX-Request": "true",
    "HX-Current-URL": "http://127.0.0.1:8885/",
    "HX-Target": "result",
    "mid": "welcome",
    "params": {}
  }
}
```

The current implementation returns an HTML wrapper containing the file content:

```html
<div id="result"><p>Hello from EasyWordy.</p></div>
```

When using HTMX, provide the corresponding target element and load a compatible
WebSocket extension in the initial page. A plain WebSocket client receives the
HTML but must handle rendering itself.

`HX-Target` is an element ID in this message, without a leading `#`.

Additional top-level message fields are collected as form fields, except for
the reserved `HEADERS`, `content`, and `hxid-1` fields. The latter is used by
the demonstration message path.

See `Wapp.HtmxSupport` for the exact decoder.

## Fuddle/Elm and native Haskell execution

For a compiled application, the library list associates an Elm module name
with its JavaScript bundle:

```yaml
libs:
  - Application: "build/application.js"

functions:
  - id: "welcome"
    action:
      Function:
        Application: "showWelcome"
```

These names are illustrative: the bundle must expose the expected module,
ports, and application-level function dispatcher.

The current JavaScript integration expects:

- An imported module exposing `default.Elm`.
- A named Elm module supporting `init`.
- A `recvMsg` port for invocation and native-return messages.
- A `sendOutput` port providing a JSON-encoded final result.
- A `sendMsg` port when native Haskell operations are required.

A final result has the shape:

```json
{
  "result": "ok",
  "content": "<p>Rendered application content.</p>",
  "container": null
}
```

Native requests identify:

```json
{
  "package": "example.operations",
  "action": "loadItems",
  "rcpt": "continuation-reference",
  "params": {}
}
```

EasyWordy resolves native calls through the legacy native map or the dynamic
registry using a `package.action` name. It returns the result to the Elm
application with the supplied recipient reference.

For native calls through this JavaScript path, the current implementation
requires an application-specific database pool. Define the Wapp's `db`
section when using that path:

```yaml
db:
  dbname: "example"
  host: "127.0.0.1"
  user: "example"
  password: "replace-with-local-password"
```

Notice that Wapp definitions use `password` and `dbname`, while the server's
top-level PostgreSQL configuration uses `passwd` and `dbase`.

A library entry labelled `Natives` triggers native-library loading.
Keep the JavaScript entry first: the initializer selects the first library
entry as the JavaScript module.

Native libraries share Haskell types and managed resources with the host.
Build them against a compatible GHC and dependency set. Their registration and
loading conventions are defined by the companion `ew-general` package.

## Development reload

The file watcher uses a 500 ms debounce interval and targets modifications to:

- `.js`
- `.elm`
- `.html`
- `.yaml`
- `.css`

The WebSocket handler reloads JavaScript when the modified path matches the
first configured library, then replays the most recently recorded action.

This is a development mechanism with several limits:

- Watching Elm source does not compile it; run the application's build process
  separately.
- A watched HTML or CSS change does not imply automatic browser replacement.
- The definitions watcher does not provide a completed routing-table refresh.
  Restart the server after changing definitions.
- Reload does not provide atomic multi-file deployment, rollback, or session
  migration.
- Replaying the latest action can repeat an operation with side effects.

Use a read-only rendering action when exercising reload.

## HTTP route overview

| Route | Purpose and status |
| --- | --- |
| `/wbap/stream/<wapp-uuid>` | Main Wapp WebSocket execution path. |
| `/wbap/ui/auth/signin` | Sign-in panel; requires the `app` query parameter. |
| `/wbap/ui/auth/signup` | Sign-up panel; requires the `app` query parameter. |
| `/wbap/auth/...` | Authentication and OAuth scaffolding. |
| `/wbap/user/...` | Account/profile/session scaffolding. |
| `/wbap/authz/me` | Permissions scaffold. |
| `/wbap/admin/...` | User and Wapp-membership administration scaffolding. |
| `/wbap/upload` | Experimental multipart upload handler. |
| `/demo-ws` | Demonstration page; some embedded URLs are outdated. |
| `/xsearch` | Demonstration search handler. |
| `/`, `/index.php`, `/wp-admin/...` | WordPress-facing routes. |

Use `Wapp.RouteDef`, `Demo.RouteDef`, and
`Wapp.Internal.WordPress.RouteDef` as the authoritative route declarations.

## WordPress integration

EasyWordy's original goal was to provide an evolutionary route from WordPress
toward more strongly typed application logic.

The repository contains:

- A PHP interpreter embedded through a custom SAPI and `inline-c`.
- WordPress-facing HTTP routes.
- MySQL connection management.
- Internal functions for inspecting WordPress-related resources.
- Notes on WordPress request handling in `app/Doc/wordpress.md`.

This work remains experimental. The presence of WordPress routes does not
establish compatibility with arbitrary themes, plugins, or complete WordPress
installations.

The current implementation embeds PHP in the server process. Later FUDD
design discussions about an isolated PHP capsule, per-function replacement,
and controlled migration describe future architecture rather than features
established by this revision.

## Source guide

| Location | Responsibility |
| --- | --- |
| `app/Main.hs` | CLI parsing and configuration-file selection. |
| `src/MainLogic.hs` | Runtime option preparation and command dispatch. |
| `src/Options/` and `src/Options.hs` | Configuration types, defaults, parsing, and merging. |
| `src/Commands/Server.hs` | Startup resources and server lifecycle. |
| `src/ServeApi.hs` | Servant context, middleware, and application environment. |
| `src/Routing/` | Composition of top-level route groups. |
| `src/Wapp/AppDef.hs` | Wapp definitions, resolved routes, and reply types. |
| `src/Wapp/Registry.hs` | YAML discovery and reference resolution. |
| `src/Wapp/Handlers.hs` | WebSocket lifecycle, dispatch, reload, and HTTP handlers. |
| `src/Wapp/WsRouter.hs` | Execution of resolved Wapp actions. |
| `src/Wapp/HtmxSupport.hs` | HTMX/WebSocket message decoding. |
| `src/Wapp/JSSupport.hs` | JavaScript execution and native-call bridge. |
| `src/Wapp/State.hs` and `src/Wapp/Types.hs` | Runtime and client-context types. |
| `src/Wapp/FileWatcher.hs` | File-change detection and debouncing. |
| `src/Wapp/InternalLib.hs` | Internal function registries. |
| `src/Wapp/Internal/WordPress/` | PHP and WordPress integration. |
| `src/DB/` | Database connections, operations, and statements. |
| `src/Assets/` | S3-compatible storage support. |
| `Support/users.sql` | Proposed account, session, context, and permissions schema. |
| `test/Spec.hs` | Placeholder test entry point. |

For a first code walkthrough, start with `app/Main.hs`, follow the server
command into `ServeApi`, then trace a WebSocket request through
`Wapp.Handlers`, `Wapp.WsRouter`, and `Wapp.JSSupport`.

## Known limitations

Before treating this revision as an operational service, account for the
following concrete gaps:

- WebSocket connections currently receive fabricated user/session identities.
  The captured Wapp UUID does not authenticate the caller.
- Account, OAuth, authorization, and administration handlers largely return
  placeholders rather than executing complete persisted workflows.
- `ServeApi` initializes empty cache/session maps. The updated Wapp cache
  constructed in `Wapp.Handlers` is not written back to shared state.
- Native loader results are discarded by the current initialization loop.
- The upload destination is hard-coded, and asset registration/session
  notification is unfinished.
- `Support/users.sql` and `DB.Stmts.Users` require reconciliation: for example,
  schema columns use `_fk` where several statements refer to `_id`.
- Cookie settings currently use `NotSecure`, and startup logging includes the
  MySQL configuration, including its password.
- Build paths, helper scripts, and some demonstration URLs retain
  developer-specific assumptions.

Use this revision in a controlled development environment while completing
these integrations.

## Testing and troubleshooting

After provisioning the development dependencies:

```bash
stack build
stack test
```

The current test suite only prints `Test suite not yet implemented`.
A successful `stack test` therefore does not validate runtime behavior.

For a manual check, start the server, load a known Wapp definition, connect to
its WebSocket endpoint, and invoke a read-only action. Inspect both server logs
and the returned HTML.

| Symptom | Check |
| --- | --- |
| Stack reports missing local packages | Provision or correct the paths in `stack.yaml`. |
| PHP headers or `libphp` cannot be found | Check PHP build compatibility, include paths, library paths, and runtime library discovery. |
| Configuration file cannot be loaded | Supply `--config` and use a `.yaml` filename. |
| Server cannot bind | Set `server.host` to a hostname or IP address without a URL scheme. |
| MySQL connection fails during startup | Configure `wordpress.db`; MySQL initialization is currently unconditional. |
| Wapp cannot be found | Check its UUID, definition filename, directory, and startup logs. |
| `NO MID` response | Supply `HEADERS.mid` in the WebSocket message. |
| `templatePath not found` | Check that `mid` matches a function `id`. |
| JavaScript fails to initialize | Check the first library entry, bundle path, module export, and required ports. |
| Native call reports no database pool | Configure the Wapp-specific `db` section. |
| Definition edits have no effect | Restart the server; definition refresh is unfinished. |

## Development direction

The broader FUDD design calls for:

- A supervised registry with explicit ownership of Wapp resources.
- Persistent sessions and resumable interaction contexts.
- A versioned browser/server protocol with correlated operations.
- Controlled deployment generations and reliable reload.
- Completed authentication, authorization, and account management.
- Progressive PHP/WordPress migration through isolated execution and bridges.

These are design goals. Check implementation and tests before documenting them
as available capabilities.

## License

EasyWordy is distributed under the BSD 3-Clause license.
See [LICENSE](LICENSE).

Maintainer: Hugo DesRosiers.