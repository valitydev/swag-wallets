%% -*- mode: erlang -*-
-module(swag_client_wallet_providers_api).

%% generated methods

-export([get_provider/2]).
-export([get_provider/3]).

-export([list_providers/2]).
-export([list_providers/3]).


-spec get_provider(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_provider(Endpoint, Params) ->
    get_provider(Endpoint, Params, []).

-spec get_provider(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_provider(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/providers/:providerID"),
        Params,
        get_request_spec(get_provider),
        Opts
    ), get_provider).

-spec list_providers(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_providers(Endpoint, Params) ->
    list_providers(Endpoint, Params, []).

-spec list_providers(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_providers(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/providers"),
        Params,
        get_request_spec(list_providers),
        Opts
    ), list_providers).

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


get_request_spec('get_provider') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'providerID', #{
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
get_request_spec('list_providers') ->
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
        {'residence', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Za-z]{3}$"}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('get_provider', 200) ->
    {'Provider', 'Provider'};

get_response_spec('get_provider', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_provider', 401) ->
    undefined;

get_response_spec('get_provider', 404) ->
    undefined;

get_response_spec('list_providers', 200) ->
    {'list', 'Provider'};

get_response_spec('list_providers', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_providers', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
