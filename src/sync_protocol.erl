-module(sync_protocol).
-include("sync_protocol_struct_pb.hrl").

%% APIs
-export([
  pack/1,
  unpack/1,
  peek/1,
  build_subscribe_req/2,
  build_subscribe_rep/1,
  build_sync_from/2,
  build_sync/2
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
  pack({subscribe_req,UserId, SessionKey}).

build_subscribe_rep(Code) ->
  pack({subscribe_rep,Code}).

build_sync_from(FromId, Limit) ->
  pack({sync_from,FromId,Limit}).

build_sync(NewId, Messages) ->
  pack({sync,NewId,Messages}).

%% =========================================================
%% Internal functions
%% =========================================================
encode_type(AtomType) ->
  EnumType = atom_to_enum(AtomType),
  IntType = sync_protocol_struct_pb:enum_to_int(message_type_t, EnumType),
  <<IntType:8>>.

decode_type(IntType) ->
  EnumType = sync_protocol_struct_pb:int_to_enum(message_type_t, IntType),
  enum_to_atom(EnumType).

encode_struct(Record) ->
  sync_protocol_struct_pb:encode(Record).

decode_struct(AtomType, Encoded) ->
  sync_protocol_struct_pb:decode(AtomType, Encoded).

atom_to_enum(subscribe_req    ) -> 'SYNC_SUBSCRIBE_REQ'    ;
atom_to_enum(subscribe_rep    ) -> 'SYNC_SUBSCRIBE_REP'    ;
atom_to_enum(unsubscribe      ) -> 'SYNC_UNSUBSCRIBE'      ;
atom_to_enum(client_heartbeat ) -> 'SYNC_CLIENT_HEARTBEAT' ;
atom_to_enum(server_heartbeat ) -> 'SYNC_SERVER_HEARTBEAT' ;
atom_to_enum(sync_from        ) -> 'SYNC_FROM'             ;
atom_to_enum(sync             ) -> 'SYNC'                  .

enum_to_atom('SYNC_SUBSCRIBE_REQ'   ) -> subscribe_req     ;
enum_to_atom('SYNC_SUBSCRIBE_REP'   ) -> subscribe_rep     ;
enum_to_atom('SYNC_UNSUBSCRIBE'     ) -> unsubscribe       ;
enum_to_atom('SYNC_CLIENT_HEARTBEAT') -> client_heartbeat  ;
enum_to_atom('SYNC_SERVER_HEARTBEAT') -> server_heartbeat  ;
enum_to_atom('SYNC_FROM'            ) -> sync_from         ;
enum_to_atom('SYNC'                 ) -> sync              .

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

x01_test() ->
  Bin = iolist_to_binary(build_subscribe_req(1,"ssxx")),
  ?assertEqual(unpack(Bin), {subscribe_req,1,"ssxx"}).

x02_test() ->
  R = [{event, 1, <<"hello">>, 234}, {event, 2, <<"world">>, 789}],
  G = iolist_to_binary(sync_protocol:build_sync(123, R)),
  D = sync_protocol:unpack(G),
  ?assertEqual(D, {sync, 123,
    [{event, 1, <<"hello">>, 234},
     {event, 2, <<"world">>, 789}]}
  ).

x03_test() ->
  Bin = iolist_to_binary(build_subscribe_req(1,"ssxx")),
  ?assertEqual(peek(Bin), subscribe_req).


-endif.


