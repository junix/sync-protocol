-module(maxwell_protocol_sync).
-include("maxwell_protocol_sync_struct_pb.hrl").

%% APIs
-export([
  pack/1,
  unpack/1,
  peek/1,
  build_subscribe_req/2,
  build_subscribe_rep/2,

  build_sync_from/2,
  build_sync_data/2
]).

%% =========================================================
%% API implementations
%% =========================================================

peek(<<IntType:8, _Rest/binary>>) ->
  decode_type(IntType).

pack(Record) ->
  EncodedType = encode_type(element(1, Record)),
  Encoded = encode_struct(Record),
  [EncodedType | Encoded].

unpack(<<IntType:8, Rest/binary>>) ->
  AtomType = decode_type(IntType),
  decode_struct(AtomType, Rest).

build_subscribe_req(UserId,SessionKey) ->
  pack({subscribe_req_t,UserId, SessionKey}).

build_subscribe_rep(Code,ErrMsg) ->
  pack({subscribe_rep_t,Code, ErrMsg}).

build_sync_from(FromId, Limit) ->
  pack({sync_from_t,FromId,Limit}).

build_sync_data(NewId, Messages) ->
  pack({sync_data_t,NewId,Messages}).

%% =========================================================
%% Internal functions
%% =========================================================
encode_type(AtomType) ->
  EnumType = atom_to_enum(AtomType),
  IntType = maxwell_protocol_sync_struct_pb:enum_to_int(message_type_t, EnumType),
  <<IntType:8>>.

decode_type(IntType) ->
  EnumType = maxwell_protocol_sync_struct_pb:int_to_enum(message_type_t, IntType),
  enum_to_atom(EnumType).

encode_struct(Record) ->
  maxwell_protocol_sync_struct_pb:encode(Record).

decode_struct(AtomType, Encoded) ->
  maxwell_protocol_sync_struct_pb:decode(AtomType, Encoded).

atom_to_enum(subscribe_req_t           ) -> 'SYNC_SUBSCRIBE_REQ'      ;
atom_to_enum(subscribe_rep_t           ) -> 'SYNC_SUBSCRIBE_REP'      ;
atom_to_enum(unsubscribe_t             ) -> 'SYNC_UNSUBSCRIBE'        ;
atom_to_enum(client_heartbeat_t        ) -> 'SYNC_CLIENT_HEARTBEAT'   ;
atom_to_enum(server_heartbeat_t        ) -> 'SYNC_SERVER_HEARTBEAT'   ;
atom_to_enum(sync_from_t               ) -> 'SYNC_FROM'               ;
atom_to_enum(sync_data_t               ) -> 'SYNC_DATA'               .

enum_to_atom('SYNC_SUBSCRIBE_REQ'    ) -> subscribe_req_t             ;
enum_to_atom('SYNC_SUBSCRIBE_REP'    ) -> subscribe_rep_t             ;
enum_to_atom('SYNC_UNSUBSCRIBE'      ) -> unsubscribe_t               ;
enum_to_atom('SYNC_CLIENT_HEARTBEAT' ) -> client_heartbeat_t          ;
enum_to_atom('SYNC_SERVER_HEARTBEAT' ) -> server_heartbeat_t          ;
enum_to_atom('SYNC_FROM'             ) -> sync_from_t                 ;
enum_to_atom('SYNC_DATA'             ) -> sync_data_t                 .

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

x01_test() ->
  Bin = iolist_to_binary(build_subscribe_req(1,"ssxx")),
  ?assertEqual(unpack(Bin), {subscribe_req_t,1,"ssxx"}).

x02_test() ->
  R = [{sync_event_t, 1, <<"hello">>, 234}, {sync_event_t, 2, <<"world">>, 789}],
  G = iolist_to_binary(maxwell_protocol_sync:build_sync_data(123, R)),
  D = maxwell_protocol_sync:unpack(G),
  ?assertEqual(D, {sync_data_t, 123,
    [{sync_event_t, 1, <<"hello">>, 234},
     {sync_event_t, 2, <<"world">>, 789}]}
  ).

x03_test() ->
  Bin = iolist_to_binary(build_subscribe_req(1,"ssxx")),
  ?assertEqual(peek(Bin), subscribe_req_t).


-endif.


