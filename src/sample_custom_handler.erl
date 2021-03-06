-module(sample_custom_handler).

-include("dns.hrl").
-include("erldns.hrl").

-behavior(gen_server).

-export([start_link/0, handle/3, filter/1]).

% Gen server hooks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DNS_SAMPLE_TYPE, 20001).

-record(state, {}).

%% API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle(Qname, Qtype, Records) ->
  gen_server:call(?MODULE, {handle, Qname, Qtype, Records}).

filter(Records) ->
  gen_server:call(?MODULE, {filter, Records}).

%% Gen server hooks
init([]) ->
  erldns_handler:register_handler([?DNS_TYPE_A], ?MODULE),
  {ok, #state{}}.

handle_call({handle, _Qname, _Qtype, Records}, _From, State) ->
  SampleRecords = lists:filter(type_match(), Records),
  NewRecords = lists:flatten(lists:map(convert(), SampleRecords)),
  {reply, NewRecords, State};

handle_call({filter, Records}, _From, State) ->
  TypeMatchFunction = type_match(),
  ConvertFunction = convert(),
  NewRecords = lists:flatten(lists:map(
      fun(R) ->
          case TypeMatchFunction(R) of
            true -> ConvertFunction(R);
            false -> R
          end
      end, Records)),
  {reply, NewRecords, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

%% Internal functions

type_match() -> fun(Record) -> Record#dns_rr.type =:= ?DNS_SAMPLE_TYPE end.

convert() -> 
  fun(Record) ->
      {ok, Address} = inet_parse:address(binary_to_list(Record#dns_rr.data)),
      Record#dns_rr{type = ?DNS_TYPE_A, data = #dns_rrdata_a{ip=Address}}
  end.
