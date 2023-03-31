%% -*- mode: erlang -*-
-module(swag_client_wallet_deposits_api).

%% generated methods

-export([list_deposit_adjustments/2]).
-export([list_deposit_adjustments/3]).

-export([list_deposit_reverts/2]).
-export([list_deposit_reverts/3]).

-export([list_deposits/2]).
-export([list_deposits/3]).


-spec list_deposit_adjustments(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposit_adjustments(Endpoint, Params) ->
    list_deposit_adjustments(Endpoint, Params, []).

-spec list_deposit_adjustments(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposit_adjustments(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/deposit-adjustments"),
        Params,
        get_request_spec(list_deposit_adjustments),
        Opts
    ), list_deposit_adjustments).

-spec list_deposit_reverts(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposit_reverts(Endpoint, Params) ->
    list_deposit_reverts(Endpoint, Params, []).

-spec list_deposit_reverts(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposit_reverts(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/deposit-reverts"),
        Params,
        get_request_spec(list_deposit_reverts),
        Opts
    ), list_deposit_reverts).

-spec list_deposits(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposits(Endpoint, Params) ->
    list_deposits(Endpoint, Params, []).

-spec list_deposits(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposits(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/deposits"),
        Params,
        get_request_spec(list_deposits),
        Opts
    ), list_deposits).

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


get_request_spec('list_deposit_adjustments') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'walletID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'identityID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'depositID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 50}, {min_length, 1}, true
, {required, false}]
        }},
        {'sourceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['Pending', 'Succeeded', 'Failed']}, true
, {required, false}]
        }},
        {'createdAtFrom', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'createdAtTo', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'amountFrom', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'amountTo', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'currencyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('list_deposit_reverts') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'walletID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'identityID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'depositID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 50}, {min_length, 1}, true
, {required, false}]
        }},
        {'sourceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['Pending', 'Succeeded', 'Failed']}, true
, {required, false}]
        }},
        {'createdAtFrom', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'createdAtTo', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'amountFrom', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'amountTo', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'currencyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('list_deposits') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'walletID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'identityID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'depositID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 50}, {min_length, 1}, true
, {required, false}]
        }},
        {'sourceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['Pending', 'Succeeded', 'Failed']}, true
, {required, false}]
        }},
        {'createdAtFrom', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'createdAtTo', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'revertStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['None', 'Partial', 'Full']}, true
, {required, false}]
        }},
        {'amountFrom', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'amountTo', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'currencyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
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


get_response_spec('list_deposit_adjustments', 200) ->
    {'inline_response_200', 'inline_response_200'};

get_response_spec('list_deposit_adjustments', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_deposit_adjustments', 401) ->
    undefined;

get_response_spec('list_deposit_reverts', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('list_deposit_reverts', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_deposit_reverts', 401) ->
    undefined;

get_response_spec('list_deposits', 200) ->
    {'inline_response_200_2', 'inline_response_200_2'};

get_response_spec('list_deposits', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_deposits', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
