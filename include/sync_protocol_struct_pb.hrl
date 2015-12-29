-ifndef(SYNC_AGENT_ID_T_PB_H).
-define(SYNC_AGENT_ID_T_PB_H, true).
-record(sync_agent_id_t, {
    user_id = erlang:error({required, user_id}),
    agent_key = erlang:error({required, agent_key})
}).
-endif.

-ifndef(SYNC_SESSION_ID_T_PB_H).
-define(SYNC_SESSION_ID_T_PB_H, true).
-record(sync_session_id_t, {
    user_id = erlang:error({required, user_id}),
    session_key = erlang:error({required, session_key})
}).
-endif.

-ifndef(SYNC_SUBSCRIBE_REQ_T_PB_H).
-define(SYNC_SUBSCRIBE_REQ_T_PB_H, true).
-record(sync_subscribe_req_t, {
    session_id = erlang:error({required, session_id})
}).
-endif.

-ifndef(SYNC_SUBSCRIBE_REP_T_PB_H).
-define(SYNC_SUBSCRIBE_REP_T_PB_H, true).
-record(sync_subscribe_rep_t, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(SYNC_UNSUBSCRIBE_REQ_T_PB_H).
-define(SYNC_UNSUBSCRIBE_REQ_T_PB_H, true).
-record(sync_unsubscribe_req_t, {
    session_id = erlang:error({required, session_id})
}).
-endif.

-ifndef(SYNC_CLIENT_HEARTBEAT_T_PB_H).
-define(SYNC_CLIENT_HEARTBEAT_T_PB_H, true).
-record(sync_client_heartbeat_t, {
    
}).
-endif.

-ifndef(SYNC_SERVER_HEARTBEAT_T_PB_H).
-define(SYNC_SERVER_HEARTBEAT_T_PB_H, true).
-record(sync_server_heartbeat_t, {
    
}).
-endif.

-ifndef(SYNC_MSG_T_PB_H).
-define(SYNC_MSG_T_PB_H, true).
-record(sync_msg_t, {
    id = erlang:error({required, id}),
    payload = erlang:error({required, payload}),
    date_added = erlang:error({required, date_added})
}).
-endif.

-ifndef(SYNC_FROM_T_PB_H).
-define(SYNC_FROM_T_PB_H, true).
-record(sync_from_t, {
    from = erlang:error({required, from}),
    limit = erlang:error({required, limit})
}).
-endif.

-ifndef(SYNC_T_PB_H).
-define(SYNC_T_PB_H, true).
-record(sync_t, {
    latest_id = erlang:error({required, latest_id}),
    msgs = []
}).
-endif.

