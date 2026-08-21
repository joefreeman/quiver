use crate::common::quiver;

// The socket round-trip test lives in its own test binary deliberately: cargo runs
// test binaries serially, so this one runs without sibling-test thread contention.
// The harness's virtual clock free-runs while the environment is idle (~1 virtual ms
// per idle step), which turns every virtual-time window in the live stack into a
// real-time race — the tightest being `handle`'s 5000-virtual-ms mount await (~50ms
// of real idle). Alongside 31 parallel sibling tests those races flake; alone they
// have orders-of-magnitude headroom.

#[test]
fn test_socket_round_trip_with_parking() {
    // The full stack over a real socket: GET (the app spawns its view, which mounts
    // once; the app parks behind the page's token), then an upgrade pipelining the
    // token and a first event in the same bytes — the warm attach is SILENT (an "h"
    // root-replace here would mean the held frame didn't match) and the buffered
    // event is pumped, so the first frame read is its patch. An abrupt TCP drop
    // re-parks the app; a reconnect with the same token re-attaches silently with
    // state preserved — the second event's patch says "2", not a fresh mount's "1".
    // The readers bail on EOF (an empty read), so a prematurely-closed socket shows
    // up as a short byte compare rather than a hung evaluation.
    quiver()
        .with_io()
        // Real sockets need the real clock: on the virtual one, the `![50]`/`![100]`
        // waits and `handle`'s mount await elapse in ~1% of their real duration and
        // race the actual TCP round-trips.
        .with_real_time()
        // Real waits (server startup, drop observation) plus a heavyweight compile:
        // headroom for a loaded test machine, where the default 5s has flaked.
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            'ev = Inc
            counter = [
              mount: #[] { 0 },
              update: #[(state): 'int, (event): 'ev] { %num.add [$state, 1] },
              view: #'int { %str.from_int $ ~> %html{ <p>Count: {~}</p> } },
              decode: %data.decode<Ev['ev]>,
            ]
            counterc = %html/live.component counter
            a = %html/live.app [root: counterc]
            handler = #'%http { %html/live.handle [$, a] }
            @[] { [port: 4186, handler: handler] ~> %http/server.serve ~ } []
            { ![50] | Ok }

            read_page = #[\TcpSocket, 'bin] {
              =[sock, acc]
              {
                | Str[acc] ~> %str.contains? [~, "</html>"] => acc
                | {
                  __tcp_socket_read__ [sock, 65536] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d]]
                  }
                }
              }
            }
            read_n = #[\TcpSocket, 'bin, 'int] {
              =[sock, acc, n]
              {
                | __integer_compare__ [%bin.length acc, n] ~> =(0 | 1) => acc
                | {
                  __tcp_socket_read__ [sock, 8192] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d], n]
                  }
                }
              }
            }

            // GET: the dead render; the app parks behind the page's token.
            [<7f000001>, 4186] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s1
            __tcp_socket_write__ [s1, "GET / HTTP/1.1\r\nHost: t\r\n\r\n" ~> .0]
            page = read_page [s1, <>] ~> Str[~]
            s1 ~> __tcp_socket_close__ ~
            %str.index_of [page, "data-q-token=\""] ~> =('int)i
            tok = %str.slice [page, %num.add [i, 14], %num.add [i, 46]]

            up = "GET / HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
            tokf = %http/websocket.encode_masked_frame [1, tok ~> .0, <00000000>]
            evf = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Inc]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]

            // Upgrade + token + event in one write. The 101 response is 129 bytes and
            // the patch frame 25; reading to exactly 154 asserts the attach's silence
            // (an "h" root-replace would arrive first and fail the byte compare).
            [<7f000001>, 4186] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s2
            __tcp_socket_write__ [s2, up ~> .0 ~> %bin.concat [~, tokf] ~> %bin.concat [~, evf]]
            h1 = read_n [s2, <>, 154]

            // Abrupt drop (no ws Close): the app re-parks. Reconnect, same token.
            s2 ~> __tcp_socket_close__ ~
            { ![100] | Ok }
            [<7f000001>, 4186] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s3
            __tcp_socket_write__ [s3, up ~> .0 ~> %bin.concat [~, tokf] ~> %bin.concat [~, evf]]
            h2 = read_n [s3, <>, 154]
            s3 ~> __tcp_socket_close__ ~

            [
              Str[%bin.slice [h1, 0, 12]],
              h1 ~> %bin.slice [~, 129, 154] ~> %bin.to_hex ~,
              Str[%bin.slice [h2, 0, 12]],
              h2 ~> %bin.slice [~, 129, 154] ~> %bin.to_hex ~,
            ]
            "#,
        )
        // The two hex strings are unmasked text frames (<81> <17> + 23 bytes) carrying
        // `[1,"0",[["t",[0],"1"]]]` and — state preserved across the reconnect — `…"2"…`.
        .expect(
            r#"["HTTP/1.1 101", "81175b312c2230222c5b5b2274222c5b305d2c2231225d5d5d", "HTTP/1.1 101", "81175b312c2230222c5b5b2274222c5b305d2c2232225d5d5d"]"#,
        );
}

#[test]
fn test_nested_child_view_over_socket() {
    // Boundaries end to end, with settle-wait: the dead render COMPOSES the
    // child's settled first frame into its boundary markers, so the page ships
    // complete (the SEO/no-JS posture) and nothing flushes at attach. The parent
    // and child DELIBERATELY collide on the `Ev[Bump]` payload: routing by vid means
    // each decodes independently. Dropping the child's key kills it — after the
    // parent's removal patch, an event to the dead vid is dropped, and the next
    // frame on the wire is provably the parent's.
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            'cev = Bump
            cc = [
              mount: #'int,
              update: #[(state): 'int, (event): 'cev] { %num.add [$state, 1] },
              view: #'int { %str.from_int $ ~> %html{ <em>{~}</em> } },
              decode: %data.decode<Ev['cev]>,
            ]
            ccf = %html/live.component cc
            'pstate = [n: 'int, kid?: (Ok | [])]
            'pev = Bump | Drop
            parent = %html/live.component [
              mount: #'int { [n: $, kid?: Ok] },
              update: #[(state): 'pstate, (event): 'pev] {
                $event ~> {
                  | =Bump => [n: %num.add [$state.n, 1], kid?: $state.kid?]
                  | [n: $state.n, kid?: []]
                }
              },
              view: #'pstate {
                s = $
                %html{ <div><p>{%str.from_int s.n}</p>{ { | s.kid? ~> =Ok => ccf [init: 100, key: "w"] | [] } }</div> }
              },
              decode: %data.decode<Ev['pev]>,
            ]
            a = %html/live.app [root: parent, init: #'%http { 0 }]
            handler = #'%http { %html/live.handle [$, a] }
            @[] { [port: 4187, handler: handler] ~> %http/server.serve ~ } []
            { ![50] | Ok }

            read_to = #[\TcpSocket, 'bin, Str['bin]] {
              =[sock, acc, needle]
              {
                | Str[acc] ~> %str.contains? [~, needle] => acc
                | {
                  __tcp_socket_read__ [sock, 65536] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d], needle]
                  }
                }
              }
            }

            [<7f000001>, 4187] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s1
            __tcp_socket_write__ [s1, "GET / HTTP/1.1\r\nHost: t\r\n\r\n" ~> .0]
            page = read_to [s1, <>, "</html>"] ~> Str[~]
            s1 ~> __tcp_socket_close__ ~
            %str.index_of [page, "data-q-token=\""] ~> =('int)i
            tok = %str.slice [page, %num.add [i, 14], %num.add [i, 46]]

            up = "GET / HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
            tokf = %http/websocket.encode_masked_frame [1, tok ~> .0, <00000000>]
            [<7f000001>, 4187] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s2
            __tcp_socket_write__ [s2, up ~> .0 ~> %bin.concat [~, tokf]]

            // The colliding payload routes by vid: the child's Bump bumps 100 -> 101…
            ev_child = %http/websocket.encode_masked_frame [1, "[\"0.1\",\"Ev[Bump]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            __tcp_socket_write__ [s2, ev_child]
            r1 = read_to [s2, <>, "[1,\"0.1\",[[\"t\",[0],\"101\"]]]"] ~> Str[~]

            // …and the parent's Bump bumps its own 0 -> 1, untouched by the child's.
            ev_parent = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Bump]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            __tcp_socket_write__ [s2, ev_parent]
            r2 = read_to [s2, <>, "[1,\"0\",[[\"t\",[0],\"1\"]]]"] ~> Str[~]

            // Dropping the key kills the child; the parent's removal patch lands…
            ev_drop = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Drop]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            __tcp_socket_write__ [s2, ev_drop]
            r3 = read_to [s2, <>, "[1,\"0\",[[\"h\",[1],\"\"]]]"] ~> Str[~]

            // …and a late event to the dead vid is dropped: the NEXT frame on the
            // wire is the parent's, with no "0.1" frame in front of it.
            __tcp_socket_write__ [s2, ev_child]
            __tcp_socket_write__ [s2, ev_parent]
            r4 = read_to [s2, <>, "[1,\"0\",[[\"t\",[0],\"2\"]]]"] ~> Str[~]
            s2 ~> __tcp_socket_close__ ~

            [
              page ~> %str.contains? [~, "<!--v:0.1--><em><!--q:0-->100<!--/q:0--></em><!--/v:0.1-->"] ~> { =Ok => Composed | NotComposed },
              r1 ~> %str.contains? [~, "[1,\"0.1\",[[\"t\",[0],\"101\"]]]"] ~> { =Ok => ChildBumped | NoChildPatch },
              r4 ~> %str.contains? [~, "0.1"] ~> { =Ok => DeadChildLeaked | DeadChildSilent },
            ]
            "#,
        )
        .expect(r#"[Composed, ChildBumped, DeadChildSilent]"#);
}

#[test]
fn test_child_crash_restart_and_budget() {
    // Supervision: a crashed child restarts IN PLACE — same vid, so the fresh
    // instance's first frame replaces the boundary interior with no parent patch —
    // and its state provably resets to the init (bump to 101 first; the restart
    // frame says 100). Past the restart budget (3), the boundary renders the crash
    // fallback and the view stays down: further events to its vid are dropped, and
    // the next frame on the wire is provably the parent's.
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            'cev = Bump | Boom
            cc = [
              mount: #'int,
              update: #[(state): 'int, (event): 'cev] {
                $event ~> {
                  | =Bump => %num.add [$state, 1]
                  | "widget crashed" ~> __panic__ ~
                }
              },
              view: #'int { %str.from_int $ ~> %html{ <em>{~}</em> } },
              decode: %data.decode<Ev['cev]>,
            ]
            ccf = %html/live.component cc
            'pev = Bump
            parent = %html/live.component [
              mount: #[] { 0 },
              update: #[(state): 'int, (event): 'pev] { %num.add [$state, 1] },
              view: #'int {
                n = $
                %html{ <div><p>{%str.from_int n}</p>{ ccf [init: 100, key: "w"] }</div> }
              },
              decode: %data.decode<Ev['pev]>,
            ]
            a = %html/live.app [root: parent]
            handler = #'%http { %html/live.handle [$, a] }
            @[] { [port: 4188, handler: handler] ~> %http/server.serve ~ } []
            { ![50] | Ok }

            read_to = #[\TcpSocket, 'bin, Str['bin]] {
              =[sock, acc, needle]
              {
                | Str[acc] ~> %str.contains? [~, needle] => acc
                | {
                  __tcp_socket_read__ [sock, 65536] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d], needle]
                  }
                }
              }
            }

            [<7f000001>, 4188] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s1
            __tcp_socket_write__ [s1, "GET / HTTP/1.1\r\nHost: t\r\n\r\n" ~> .0]
            page = read_to [s1, <>, "</html>"] ~> Str[~]
            s1 ~> __tcp_socket_close__ ~
            %str.index_of [page, "data-q-token=\""] ~> =('int)i
            tok = %str.slice [page, %num.add [i, 14], %num.add [i, 46]]

            up = "GET / HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
            tokf = %http/websocket.encode_masked_frame [1, tok ~> .0, <00000000>]
            ev_bump = %http/websocket.encode_masked_frame [1, "[\"0.1\",\"Ev[Bump]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            ev_boom = %http/websocket.encode_masked_frame [1, "[\"0.1\",\"Ev[Boom]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            ev_parent = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Bump]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]

            [<7f000001>, 4188] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s2
            __tcp_socket_write__ [s2, up ~> .0 ~> %bin.concat [~, tokf]]

            __tcp_socket_write__ [s2, ev_bump]
            r1 = read_to [s2, <>, "[1,\"0.1\",[[\"t\",[0],\"101\"]]]"] ~> Str[~]

            // Crash 1..3: each within budget — the restart frame shows the INIT (100).
            restart_needle = "[1,\"0.1\",[[\"h\",[],\"<em><!--q:0-->100<!--/q:0--></em>\"]]]"
            __tcp_socket_write__ [s2, ev_boom]
            r2 = read_to [s2, <>, restart_needle] ~> Str[~]
            __tcp_socket_write__ [s2, ev_boom]
            r3 = read_to [s2, <>, restart_needle] ~> Str[~]
            __tcp_socket_write__ [s2, ev_boom]
            r4 = read_to [s2, <>, restart_needle] ~> Str[~]

            // Crash 4: budget exhausted — the fallback lands and the view stays down.
            __tcp_socket_write__ [s2, ev_boom]
            r5 = read_to [s2, <>, "view crashed"] ~> Str[~]

            // Events to the dead vid are dropped; the next frame is the parent's.
            __tcp_socket_write__ [s2, ev_bump]
            __tcp_socket_write__ [s2, ev_parent]
            r6 = read_to [s2, <>, "[1,\"0\",[[\"t\",[0],\"1\"]]]"] ~> Str[~]
            s2 ~> __tcp_socket_close__ ~

            [
              r1 ~> %str.contains? [~, "[1,\"0.1\",[[\"t\",[0],\"101\"]]]"] ~> { =Ok => Bumped | NoBump },
              r5 ~> %str.contains? [~, "[1,\"0.1\",[[\"h\",[],\"<em>view crashed</em>\"]]]"] ~> { =Ok => FellBack | NoFallback },
              r6 ~> %str.contains? [~, "0.1"] ~> { =Ok => DeadChildLeaked | DeadChildSilent },
            ]
            "#,
        )
        .expect(r#"[Bumped, FellBack, DeadChildSilent]"#);
}

#[test]
fn test_live_navigation_patches_root() {
    // Live navigation (nav-0, params-patch): the root's state is derived from the URL
    // path, and a `["nav", target]` message re-derives it via the component's `nav`
    // handler — no remount — patching the view in place over the same socket. The dead
    // render already routes by path (the GET for /home renders "home"), so nav only adds
    // the socket-preserving transition: after ["nav","/posts"] the view patches to "posts".
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            'cev = Bump
            seg0 = #'%http { $.path ~> { | =Cons[Str[s], _] => Str[s] | "home" } }
            root = %html/live.component [
              mount: #'%http { seg0 $ },
              update: #[(state): Str['bin], (event): 'cev] { $state },
              view: #Str['bin] { %html{ <p>{$}</p> } },
              decode: #'%str { [] },
              nav: #[(state): Str['bin], (request): '%http] { seg0 $request },
            ]
            a = %html/live.app [root: root, init: #'%http { $ }]
            handler = #'%http { %html/live.handle [$, a] }
            @[] { [port: 4189, handler: handler] ~> %http/server.serve ~ } []
            { ![50] | Ok }

            read_to = #[\TcpSocket, 'bin, Str['bin]] {
              =[sock, acc, needle]
              {
                | Str[acc] ~> %str.contains? [~, needle] => acc
                | {
                  __tcp_socket_read__ [sock, 65536] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d], needle]
                  }
                }
              }
            }

            [<7f000001>, 4189] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s1
            __tcp_socket_write__ [s1, "GET /home HTTP/1.1\r\nHost: t\r\n\r\n" ~> .0]
            page = read_to [s1, <>, "</html>"] ~> Str[~]
            s1 ~> __tcp_socket_close__ ~
            %str.index_of [page, "data-q-token=\""] ~> =('int)i
            tok = %str.slice [page, %num.add [i, 14], %num.add [i, 46]]

            up = "GET /home HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
            tokf = %http/websocket.encode_masked_frame [1, tok ~> .0, <00000000>]
            [<7f000001>, 4189] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s2
            __tcp_socket_write__ [s2, up ~> .0 ~> %bin.concat [~, tokf]]

            // Navigate to /posts: the root re-derives from the new path and patches "home" -> "posts".
            ev_nav = %http/websocket.encode_masked_frame [1, "[\"nav\",\"/posts\"]" ~> .0, <00000000>]
            __tcp_socket_write__ [s2, ev_nav]
            r1 = read_to [s2, <>, "[1,\"0\",[[\"t\",[0],\"posts\"]]]"] ~> Str[~]
            s2 ~> __tcp_socket_close__ ~

            [
              page ~> %str.contains? [~, "<p><!--q:0-->home<!--/q:0--></p>"] ~> { =Ok => MountedHome | NoHome },
              r1 ~> %str.contains? [~, "[1,\"0\",[[\"t\",[0],\"posts\"]]]"] ~> { =Ok => Navigated | NoNav },
            ]
            "#,
        )
        .expect(r#"[MountedHome, Navigated]"#);
}

#[test]
fn test_server_redirect_syncs_url() {
    // Server-initiated navigation: an `update` marks its result with `%html/live.redirect`.
    // The view patches the page as usual AND emits a `[2, path]` frame, which the client
    // applies as a pushState — the address bar follows a view change the server made with
    // no link click. Here a "jump" event redirects to /posts/7.
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            'st = [page: Str['bin]]
            'ev = Jump
            root = %html/live.component [
              mount: #'%http { [page: "home"] },
              update: #[(state): 'st, (event): 'ev] {
                [page: "seven"] ~> %html/live.redirect [~, "/posts/7"]
              },
              view: #'st { %html{ <p>{$.page}</p> } },
              decode: %data.decode<Ev['ev]>,
            ]
            a = %html/live.app [root: root, init: #'%http { $ }]
            handler = #'%http { %html/live.handle [$, a] }
            @[] { [port: 4190, handler: handler] ~> %http/server.serve ~ } []
            { ![50] | Ok }

            read_to = #[\TcpSocket, 'bin, Str['bin]] {
              =[sock, acc, needle]
              {
                | Str[acc] ~> %str.contains? [~, needle] => acc
                | {
                  __tcp_socket_read__ [sock, 65536] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d], needle]
                  }
                }
              }
            }

            [<7f000001>, 4190] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s1
            __tcp_socket_write__ [s1, "GET / HTTP/1.1\r\nHost: t\r\n\r\n" ~> .0]
            page = read_to [s1, <>, "</html>"] ~> Str[~]
            s1 ~> __tcp_socket_close__ ~
            %str.index_of [page, "data-q-token=\""] ~> =('int)i
            tok = %str.slice [page, %num.add [i, 14], %num.add [i, 46]]

            up = "GET / HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
            tokf = %http/websocket.encode_masked_frame [1, tok ~> .0, <00000000>]
            [<7f000001>, 4190] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s2
            __tcp_socket_write__ [s2, up ~> .0 ~> %bin.concat [~, tokf]]

            // The jump event: the server sends a URL-sync frame and patches the view.
            ev_jump = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Jump]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            __tcp_socket_write__ [s2, ev_jump]
            r1 = read_to [s2, <>, "[1,\"0\",[[\"t\",[0],\"seven\"]]]"] ~> Str[~]
            s2 ~> __tcp_socket_close__ ~

            [
              r1 ~> %str.contains? [~, "[2,\"/posts/7\"]"] ~> { =Ok => UrlSynced | NoSync },
              r1 ~> %str.contains? [~, "[1,\"0\",[[\"t\",[0],\"seven\"]]]"] ~> { =Ok => Patched | NoPatch },
            ]
            "#,
        )
        .expect(r#"[UrlSynced, Patched]"#);
}

#[test]
fn test_stale_redirect_mark_is_not_resynced() {
    // A `:redirect` mark rides the state VALUE, so an update that answers the state
    // unchanged (the framework's own decode-drop path does too) still carries the old
    // mark — naively re-read, it would re-sync the address bar on every later event
    // (duplicate history entries; a stale yank after the user navigates away). The view
    // tracks the last-synced path: an unchanged mark is suppressed, an absent mark
    // resets the tracker so a later redirect to the same path fires again. Here jump
    // marks /posts/7 (sync 1), noop answers the state unchanged (suppressed), two
    // builds fresh state (reset), the second jump legitimately re-redirects (sync 2),
    // fin bounds the read: exactly two [2,"/posts/7"] frames.
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            'st = [page: Str['bin]]
            'ev = Jump | Noop | Two | Fin
            root = %html/live.component [
              mount: #'%http { [page: "home"] },
              update: #[(state): 'st, (event): 'ev] {
                $event ~> {
                  | =Jump => [page: "seven"] ~> %html/live.redirect [~, "/posts/7"]
                  | =Noop => $state
                  | =Two => [page: "two"]
                  | [page: "fin"]
                }
              },
              view: #'st { %html{ <p>{$.page}</p> } },
              decode: %data.decode<Ev['ev]>,
            ]
            a = %html/live.app [root: root, init: #'%http { $ }]
            handler = #'%http { %html/live.handle [$, a] }
            @[] { [port: 4191, handler: handler] ~> %http/server.serve ~ } []
            { ![50] | Ok }

            read_to = #[\TcpSocket, 'bin, Str['bin]] {
              =[sock, acc, needle]
              {
                | Str[acc] ~> %str.contains? [~, needle] => acc
                | {
                  __tcp_socket_read__ [sock, 65536] ~> { | =('bin)v => v | <> } ~> =d
                  {
                    | __integer_compare__ [%bin.length d, 0] ~> =0 => acc
                    | ^ [sock, %bin.concat [acc, d], needle]
                  }
                }
              }
            }

            count = #[Str['bin], Str['bin], 'int] {
              =[h, n, acc]
              %str.index_of [h, n] ~> {
                | =('int)i => {
                  h ~> =Str[hb]
                  ^ [%str.slice [h, %num.add [i, 1], %bin.length hb], n, %num.add [acc, 1]]
                }
                | acc
              }
            }

            [<7f000001>, 4191] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s1
            __tcp_socket_write__ [s1, "GET / HTTP/1.1\r\nHost: t\r\n\r\n" ~> .0]
            page = read_to [s1, <>, "</html>"] ~> Str[~]
            s1 ~> __tcp_socket_close__ ~
            %str.index_of [page, "data-q-token=\""] ~> =('int)i
            tok = %str.slice [page, %num.add [i, 14], %num.add [i, 46]]

            up = "GET / HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
            tokf = %http/websocket.encode_masked_frame [1, tok ~> .0, <00000000>]
            [<7f000001>, 4191] ~> __tcp_connect__ ~ ~> =(\TcpSocket)s2
            __tcp_socket_write__ [s2, up ~> .0 ~> %bin.concat [~, tokf]]

            ev_jump = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Jump]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            ev_noop = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Noop]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            ev_two = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Two]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            ev_fin = %http/websocket.encode_masked_frame [1, "[\"0\",\"Ev[Fin]\",[\"click\",0,0,0,[false,false,false,false]]]" ~> .0, <00000000>]
            __tcp_socket_write__ [s2, ev_jump]
            __tcp_socket_write__ [s2, ev_noop]
            __tcp_socket_write__ [s2, ev_two]
            __tcp_socket_write__ [s2, ev_jump]
            __tcp_socket_write__ [s2, ev_fin]

            stream = read_to [s2, <>, "[1,\"0\",[[\"t\",[0],\"fin\"]]]"] ~> Str[~]
            s2 ~> __tcp_socket_close__ ~

            Syncs[count [stream, "[2,\"/posts/7\"]", 0]]
            "#,
        )
        .expect(r#"Syncs[2]"#);
}
