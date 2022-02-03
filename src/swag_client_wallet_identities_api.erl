%% -*- mode: erlang -*-
-module(swag_client_wallet_identities_api).

%% generated methods

-export([create_identity/2]).
-export([create_identity/3]).

-export([get_identity/2]).
-export([get_identity/3]).

-export([list_identities/2]).
-export([list_identities/3]).


-spec create_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_identity(Endpoint, Params) ->
    create_identity(Endpoint, Params, []).

-spec create_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_identity(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities"),
        Params,
        get_request_spec(create_identity),
        Opts
    ), create_identity).

-spec get_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity(Endpoint, Params) ->
    get_identity(Endpoint, Params, []).

-spec get_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID"),
        Params,
        get_request_spec(get_identity),
        Opts
    ), get_identity).

-spec list_identities(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_identities(Endpoint, Params) ->
    list_identities(Endpoint, Params, []).

-spec list_identities(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_identities(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities"),
        Params,
        get_request_spec(list_identities),
        Opts
    ), list_identities).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_wallet_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: swag_client_wallet:operation_id()) ->
    Spec :: swag_client_wallet_procession:request_spec() | no_return().


get_request_spec('create_identity') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'Identity', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_identity') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'identityID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('list_identities') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'providerID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('create_identity', 201) ->
    {'Identity', 'Identity'};

get_response_spec('create_identity', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_identity', 401) ->
    undefined;

get_response_spec('create_identity', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_identity', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_identity', 200) ->
    {'Identity', 'Identity'};

get_response_spec('get_identity', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_identity', 401) ->
    undefined;

get_response_spec('get_identity', 404) ->
    undefined;

get_response_spec('list_identities', 200) ->
    {'inline_response_200_4', 'inline_response_200_4'};

get_response_spec('list_identities', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_identities', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
