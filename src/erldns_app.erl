-module(erldns_app).
-behavior(application).

% Application hooks
-export([start/2, start_phase/3, stop/1]).

start(_Type, _Args) ->
  erldns_metrics:setup(),
  random:seed(erlang:now()),
  erldns_sup:start_link().

start_phase(post_start, _StartType, _PhaseArgs) ->
  case application:get_env(erldns, custom_zone_parsers) of
    {ok, Parsers} -> erldns_zone_parser:register_parsers(Parsers);
    _ -> ok
  end,

  case application:get_env(erldns, custom_zone_encoders) of
    {ok, Encoders} -> erldns_zone_encoder:register_encoders(Encoders);
    _ -> ok
  end,

  lager:info("Loading zones from local file"),
  erldns_zone_loader:load_zones(),

  case application:get_env(erldns, zone_server) of
    {ok, _} ->
      lager:info("Loading zones from remote server"),
      erldns_zone_loader:load_remote_zones(),

      lager:info("Websocket monitor connecting"),
      erldns_zoneserver_monitor:connect();
    _ ->
      not_fetching
  end,

  erldns_events:add_handler(erldns_event_logger),

  % Start up the UDP and TCP servers now
  erldns_server_sup:start_link(),

  erldns_events:notify(servers_started),

  ok.

stop(_State) ->
  lager:info("Stop erldns application"),
  ok.
