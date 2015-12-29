-module(sync_protocol).
-include("sync_protocol_struct_pb.hrl").

%% APIs
-export([pack/1,unpack/1]).

%% =========================================================
%% API implementations
%% =========================================================

pack(Record) ->
  EncodedType = encode_type(element(1, Record)),
  Encoded = encode_struct(Record),
  [EncodedType | Encoded].

unpack(Bin) ->
  <<IntType:8, Encoded/binary>> = Bin,
  AtomType = decode_type(IntType),
  decode_struct(AtomType, Encoded).

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
atom_to_enum(sync_msg_t              ) -> 'SYNC_MSG'              ;
atom_to_enum(sync_from_t             ) -> 'SYNC_FROM'             ;
atom_to_enum(sync_t                  ) -> 'SYNC'                  .

enum_to_atom('SYNC_SUBSCRIBE_REQ'   ) -> sync_subscribe_req_t     ;
enum_to_atom('SYNC_SUBSCRIBE_REP'   ) -> sync_subscribe_rep_t     ;
enum_to_atom('SYNC_UNSUBSCRIBE'     ) -> sync_unsubscribe_t       ;
enum_to_atom('SYNC_CLIENT_HEARTBEAT') -> sync_client_heartbeat_t  ;
enum_to_atom('SYNC_SERVER_HEARTBEAT') -> sync_server_heartbeat_t  ;
enum_to_atom('SYNC_MSG'             ) -> sync_msg_t               ;
enum_to_atom('SYNC_FROM'            ) -> sync_from_t              ;
enum_to_atom('SYNC'                 ) -> sync_t                   .

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 x01_test() ->
        R = {sync_msg_t,1,<<"hv">>,345},
        Bin = iolist_to_binary(pack(R)),
        ?assertEqual(unpack(Bin),R).
-endif.


