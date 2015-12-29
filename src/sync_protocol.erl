-module(sync_protocol).
-include("sync_protocol_struct_pb.hrl").

%% APIs
-export([
  pack/1,
  unpack/1,
  build_subscribe_req/2,
  build_subscribe_rep/1,
  build_sync_from/2,
  build_sync/2
]).

%% =========================================================
%% API implementations
%% =========================================================

pack(Record) ->
  EncodedType = encode_type(element(1, Record)),
  Encoded = encode_struct(Record),
  [EncodedType | Encoded].

unpack(<<IntType:8, Rest/binary>>) ->
  AtomType = decode_type(IntType),
  decode_struct(AtomType, Rest).

build_subscribe_req(UserId,SessionKey) ->
  pack({sync_subscribe_req_t,{sync_session_id_t,UserId, SessionKey}}).

build_subscribe_rep(Code) ->
  pack({sync_subscribe_rep_t,Code}).

build_sync_from(FromId, Limit) ->
  pack({sync_from_t,FromId,Limit}).

build_sync(NewId, Messages) ->
  pack({sync_t,NewId,Messages}).

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

atom_to_enum(sync_subscribe_req_t    ) -> 'SYNC_SUBSCRIBE_REQ'    ;
atom_to_enum(sync_subscribe_rep_t    ) -> 'SYNC_SUBSCRIBE_REP'    ;
atom_to_enum(sync_unsubscribe_t      ) -> 'SYNC_UNSUBSCRIBE'      ;
atom_to_enum(sync_client_heartbeat_t ) -> 'SYNC_CLIENT_HEARTBEAT' ;
atom_to_enum(sync_server_heartbeat_t ) -> 'SYNC_SERVER_HEARTBEAT' ;
atom_to_enum(sync_from_t             ) -> 'SYNC_FROM'             ;
atom_to_enum(sync_t                  ) -> 'SYNC'                  .

enum_to_atom('SYNC_SUBSCRIBE_REQ'   ) -> sync_subscribe_req_t     ;
enum_to_atom('SYNC_SUBSCRIBE_REP'   ) -> sync_subscribe_rep_t     ;
enum_to_atom('SYNC_UNSUBSCRIBE'     ) -> sync_unsubscribe_t       ;
enum_to_atom('SYNC_CLIENT_HEARTBEAT') -> sync_client_heartbeat_t  ;
enum_to_atom('SYNC_SERVER_HEARTBEAT') -> sync_server_heartbeat_t  ;
enum_to_atom('SYNC_FROM'            ) -> sync_from_t              ;
enum_to_atom('SYNC'                 ) -> sync_t                   .

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
x01_test() ->
  Bin = iolist_to_binary(build_subscribe_req(1,"ssxx")),
  ?assertEqual(unpack(Bin), {sync_subscribe_req_t,{sync_session_id_t,1,"ssxx"}}).

x02_test() ->
  R = [{sync_msg_t, 1, <<"hello">>, 234}, {sync_msg_t, 2, <<"world">>, 789}],
  G = iolist_to_binary(sync_protocol:build_sync(123, R)),
  D = sync_protocol:unpack(G),
  ?assertEqual(D, {sync_t, 123,
    [{sync_msg_t, 1, <<"hello">>, 234},
      {sync_msg_t, 2, <<"world">>, 789}]}
  ).

-endif.


