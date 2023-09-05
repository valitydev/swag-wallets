%% -*- mode: erlang -*-
-module(swag_client_wallet_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_client_wallet:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw()))).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema = #{?DEFINITIONS := Defs}) ->
    try
        Parents = enumerate_parents(Defs),
        DefsFixed = maps:fold(fun correct_definition/3, Defs, Parents),
        Schema#{?DEFINITIONS := DefsFixed}
    catch
        _:Error ->
            handle_error(Error)
    end;
enumerate_discriminator_children(_) ->
    handle_error(no_definitions).

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

enumerate_parents(Definitions) ->
    maps:fold(
        fun
            (Name, #{<<"allOf">> := AllOf}, AccIn) ->
                lists:foldl(
                    fun
                        (#{<<"$ref">> := <<"#/definitions/", Parent/binary>>}, Acc) ->
                            Schema = maps:get(Parent, Definitions),
                            Discriminator = maps:get(<<"discriminator">>, Schema, undefined),
                            add_parent_child(Discriminator, Parent, Name, Acc);
                        (_Schema, Acc) ->
                            Acc
                    end,
                    AccIn,
                    AllOf
                );
            (Name, #{<<"discriminator">> := _}, Acc) ->
                add_parent(Name, Acc);
            (_Name, _Schema, AccIn) ->
                AccIn
        end,
        #{},
        Definitions
    ).

add_parent_child(undefined, _Parent, _Child, Acc) ->
    Acc;
add_parent_child(_Discriminator, Parent, Child, Acc) ->
    maps:put(Parent, [Child | maps:get(Parent, Acc, [])], Acc).

add_parent(Parent, Acc) when not is_map_key(Parent, Acc) ->
    maps:put(Parent, [], Acc);
add_parent(_Parent, Acc) ->
    Acc.

correct_definition(Parent, Children, Definitions) ->
    ParentSchema1 = maps:get(Parent, Definitions),
    Discriminator = maps:get(<<"discriminator">>, ParentSchema1),
    ParentSchema2 = deep_put([<<"properties">>, Discriminator, <<"enum">>], Children, ParentSchema1),
    maps:put(Parent, ParentSchema2, Definitions).

deep_put([K], V, M) ->
    M#{K => V};
deep_put([K | Ks], V, M) ->
    maps:put(K, deep_put(Ks, V, maps:get(K, M)), M).

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"swagger">> => <<"2.0">>,
  <<"info">> => #{
    <<"description">> => <<"\nThe Vality Wallet API is the base and only point of interaction with the wallet system. All system state changes are carried out by calling the corresponding API methods. Any third party applications, including our websites and other UIs, are external client applications.\nThe Vality API works on top of the HTTP protocol. We use REST architecture, the scheme is described according to [OpenAPI 2.0](https://spec.openapis.org/oas/v2.0). Return codes are described by the corresponding HTTP statuses. The system accepts and returns JSON values in the body of requests and responses.\n## Content Format\nAny API request must be encoded in UTF-8 and must contain JSON content.\n```\n  Content-Type: application/json; charset=utf-8\n```\n## Date format\nThe system accepts and returns timestamp values in the `date-time` format described in [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339):\n```\n  2017-01-01T00:00:00Z\n  2017-01-01T00:00:01+00:00\n```\n## Maximum request processing time\nIn any API call, you can pass a timeout parameter in the `X-Request-Deadline` header of the corresponding request, which determines the maximum time to wait for the operation to complete on the request:\n```\n X-Request-Deadline: 10s\n```\nAfter the specified time has elapsed, the system stops processing the request. It is recommended to specify a value of no more than one minute, but no less than three seconds.\n`X-Request-Deadline` can:\n* set in `date-time` format according to [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339);\n* specified in relative terms: in milliseconds (`150000ms`), seconds (`540s`) or minutes (`3.5m`).\n## Request processing errors\nDuring the processing of requests by our system, various unforeseen situations may occur. The system signals about their appearance via the HTTP protocol with the corresponding [statuses][5xx], indicating server errors.\n\n | Code    | Description                                                                                                                                                                                                                      |\n | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |\n | **500** | An unexpected situation occurred while the system was processing the request. If you receive such a response code, we recommend that you contact technical support.                                                              |\n | **503** | The system is temporarily unavailable and not ready to serve this request. The request is guaranteed to fail, if you receive a response code like this, try to implement it later when the system is restored to availability.   |\n | **504** | The system has exceeded the allowable request processing time, the result of the request is undefined. Try to resubmit the request or find out the result of the original request, if you do not want to re-execute the request. |\n\n[5xx]: https://tools.ietf.org/html/rfc7231#section-6.6\n">>,
    <<"version">> => <<"0.1.0">>,
    <<"title">> => <<"Vality Wallet API">>,
    <<"termsOfService">> => <<"https://vality.dev/">>,
    <<"contact">> => #{
      <<"name">> => <<"Technical support team">>,
      <<"url">> => <<"https://api.vality.dev">>,
      <<"email">> => <<"support@vality.dev">>
    }
  },
  <<"host">> => <<"api.vality.dev">>,
  <<"basePath">> => <<"/wallet/v0">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Providers">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Service providers">>
  }, #{
    <<"name">> => <<"Identities">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Identities">>
  }, #{
    <<"name">> => <<"Wallets">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Wallets">>
  }, #{
    <<"name">> => <<"Deposits">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Deposits">>
  }, #{
    <<"name">> => <<"Withdrawals">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Withdrawals">>
  }, #{
    <<"name">> => <<"Residences">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Residences">>
  }, #{
    <<"name">> => <<"Currencies">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Currencies">>
  }, #{
    <<"name">> => <<"Reports">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Reports">>
  }, #{
    <<"name">> => <<"Downloads">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"File upload">>
  }, #{
    <<"name">> => <<"W2W">>,
    <<"description">> => <<"Transfers of funds between wallets within the system">>,
    <<"x-displayName">> => <<"Transfers within the system">>
  }, #{
    <<"name">> => <<"Webhooks">>,
    <<"description">> => <<"## Vality Webhooks Management API\nThis section describes methods that allow you to manage Webhooks, or tools for receiving asynchronous notifications via HTTP requests when one or a group of events of interest to you occurs, for example, that a withdrawal within the created wallet was successfully completed.\n## Vality Webhooks Events API\nAttention! Only the Webhooks Management API is part of the Vality system and hence this specification. To implement the notification handler, you will need to read the OpenAPI specification [Vality Wallets Webhook Events API](https://vality.github.io/swag-wallets-webhook-events/).\n">>,
    <<"x-displayName">> => <<"Webhooks">>
  }, #{
    <<"name">> => <<"Error Codes">>,
    <<"description">> => <<"\n## Withdrawal errors\n| Code                   | Description                                                                                                                                                                                         | | ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | | InvalidSenderResource  | Invalid transfer source (entered the number of a non-existent card, missing account, etc.)                                                                                                          | | InvalidReceiverResource| Wrong transfer receiver (invalid card number entered, etc.)                                                                                                                                         | | InsufficientFunds      | Insufficient funds on the bank card account                                                                                                                                                         | | PreauthorizationFailed | Pre-Authorization Rejected (Wrong 3D-Secure Code Entered, Cancel Link Clicked on 3D-Secure Form)                                                                                                    | | RejectedByIssuer       | The transfer was rejected by the issuer (prohibitions were established by the country of debiting, a ban on purchases on the Internet, the withdrawal was rejected by the issuer's antifraud, etc.) |\n">>,
    <<"x-displayName">> => <<"Error codes">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/currencies/{currencyID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Currencies">> ],
        <<"summary">> => <<"Get currency description">>,
        <<"operationId">> => <<"getCurrency">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Currency character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Za-z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Currency found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Currency">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/deposit-adjustments">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Deposits">> ],
        <<"summary">> => <<"Finding adjustments">>,
        <<"operationId">> => <<"listDepositAdjustments">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Wallet identifier">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"depositID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the input of funds">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 50,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"sourceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the fund source">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        }, #{
          <<"name">> => <<"createdAtFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date from">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"createdAtTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date to">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"amountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"amountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search results">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/deposit-reverts">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Deposits">> ],
        <<"summary">> => <<"Search for reverts">>,
        <<"operationId">> => <<"listDepositReverts">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"depositID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the input of funds">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 50,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"sourceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the source of funds">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        }, #{
          <<"name">> => <<"createdAtFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date from">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"createdAtTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date to">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"amountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"amountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search result">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/deposits">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Deposits">> ],
        <<"summary">> => <<"Search for deposits">>,
        <<"operationId">> => <<"listDeposits">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"depositID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the deposit">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 50,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"sourceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the funds source">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        }, #{
          <<"name">> => <<"createdAtFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date from">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"createdAtTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date to">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"revertStatus">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"None">>, <<"Partial">>, <<"Full">> ]
        }, #{
          <<"name">> => <<"amountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"amountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search results">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_2">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/destinations">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"List of destinations">>,
        <<"operationId">> => <<"listDestinations">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's idenity">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search result">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_3">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Start a destination creation">>,
        <<"operationId">> => <<"createDestination">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"destination">>,
          <<"description">> => <<"Destination data">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Destination">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Destination created">>,
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"The URI of the created destination">>
              }
            },
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Destination">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Incorrect destination data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/destinations/{destinationID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Get a specific destination">>,
        <<"operationId">> => <<"getDestination">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"destinationID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the destination">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Destination found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Destination">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/destinations/{destinationID}/grants">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Grant the right to manage the destinations">>,
        <<"operationId">> => <<"issueDestinationGrant">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"destinationID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the destination">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"request">>,
          <<"description">> => <<"Request for the right to manage the destinations">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/DestinationGrantRequest">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"The right is granted">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DestinationGrantRequest">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for issuance">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/external-ids/destinations/{externalID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Get a destination by external identifier">>,
        <<"operationId">> => <<"getDestinationByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"External identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Destination found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Destination">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/external-ids/withdrawals/{externalID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Get withdrawal status by external identifier">>,
        <<"operationId">> => <<"getWithdrawalByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"External identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Withdrawal found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Withdrawal">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/external/wallets">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Get wallet by specified external identifier">>,
        <<"operationId">> => <<"getWalletByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"External wallet identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Wallet details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Wallet">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/files/{fileID}/download">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Downloads">> ],
        <<"description">> => <<"Get a link to download a file">>,
        <<"operationId">> => <<"downloadFile">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"fileID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The file identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Data to get file">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/FileDownload">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/identities">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"List the identities of the owners">>,
        <<"operationId">> => <<"listIdentities">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Service provider's identifier">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search result">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_4">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Create owner identity">>,
        <<"operationId">> => <<"createIdentity">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"identity">>,
          <<"description">> => <<"Data of the identity created">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Identity">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Owner identity created">>,
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"Created identity URI">>
              }
            },
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Identity">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid owner identity data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/identities/{identityID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Get the owner's identity">>,
        <<"operationId">> => <<"getIdentity">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Owner's identity found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Identity">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/identities/{identityID}/reports">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Get a list of owner identity reports for a period">>,
        <<"operationId">> => <<"getReports">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Start of the time period">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"End of the time period">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"type">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Type of reports received">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"withdrawalRegistry">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Reports found">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Report">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Generate a report with the specified type on the identity of the owner for the specified period of time">>,
        <<"operationId">> => <<"createReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"ReportParams">>,
          <<"description">> => <<"Report generation options">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/ReportParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Report created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Report">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/identities/{identityID}/reports/{reportID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Get a report for a given identifier">>,
        <<"operationId">> => <<"getReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"reportID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The report identifier">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Report found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Report">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/identities/{identityID}/withdrawal-methods">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Get withdrawal methods available by owner identity">>,
        <<"operationId">> => <<"getWithdrawalMethods">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Methods found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_5">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/providers">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"List available providers">>,
        <<"operationId">> => <<"listProviders">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"residence">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The residence within which the services are provided,\n[ISO 3166-1] country or region code (https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Za-z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Providers found">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Provider">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/providers/{providerID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Get provider details">>,
        <<"operationId">> => <<"getProvider">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the provider">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Provider found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Provider">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/residences/{residence}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Residences">> ],
        <<"summary">> => <<"Get a description of the residence region">>,
        <<"operationId">> => <<"getResidence">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"residence">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"The residence within which the services are provided,\n[ISO 3166-1] country or region code (https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Za-z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Residence region found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Residence">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/w2w/transfers">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"W2W">> ],
        <<"description">> => <<"Create a transfer">>,
        <<"operationId">> => <<"createW2WTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"transferParams">>,
          <<"description">> => <<"Transfer creation options">>,
          <<"required">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/W2WTransferParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Transfer started">>,
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI of the transfer started">>
              }
            },
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/W2WTransfer">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid transfer input data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/w2w/transfers/{w2wTransferID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"W2W">> ],
        <<"description">> => <<"Get the transfer status.">>,
        <<"operationId">> => <<"getW2WTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"w2wTransferID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of transfer">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Transfer found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/W2WTransfer">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/wallets">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"List the wallets">>,
        <<"operationId">> => <<"listWallets">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of owner's identity">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search result">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_6">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Create a new wallet">>,
        <<"operationId">> => <<"createWallet">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"wallet">>,
          <<"description">> => <<"Data of the created wallet">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Wallet">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Wallet created">>,
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI of the wallet created">>
              }
            },
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Wallet">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data of the wallet">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/wallets/{walletID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Get wallet data">>,
        <<"operationId">> => <<"getWallet">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Wallet found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Wallet">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/wallets/{walletID}/account">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Get account status">>,
        <<"operationId">> => <<"getWalletAccount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Wallet account received">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WalletAccount">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/wallets/{walletID}/grants">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Grant the right to manage funds">>,
        <<"operationId">> => <<"issueWalletGrant">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"request">>,
          <<"description">> => <<"Request for the right to manage funds on the wallet">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/WalletGrantRequest">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Single right granted">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WalletGrantRequest">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for issuance">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/webhooks">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Get list of existing webhooks.">>,
        <<"operationId">> => <<"getWebhooks">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"A list of webhooks">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Webhook">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for obtaining webhooks">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Create a new webhook.">>,
        <<"operationId">> => <<"createWebhook">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"webhookParams">>,
          <<"description">> => <<"Parameters of the created webhook">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Webhook">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Webhook created">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Webhook">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for webhook creation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/webhooks/{webhookID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Get a webhook by its identifier.">>,
        <<"operationId">> => <<"getWebhookByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"webhookID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Webhook identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Webhook's data">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Webhook">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for obtaining a webhook">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      },
      <<"delete">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Remove the specified webhook.">>,
        <<"operationId">> => <<"deleteWebhookByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"webhookID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Webhook identifier">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Webhook successfully removed">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for removing webhook">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/withdrawal-quotes">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Quote preparation">>,
        <<"description">> => <<"Fixing the exchange rate for making withdrawals with conversion">>,
        <<"operationId">> => <<"createQuote">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"withdrawalQuoteParams">>,
          <<"description">> => <<"Quote data for withdrawal">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalQuoteParams">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Received quote">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WithdrawalQuote">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for getting a quote">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/withdrawals">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Search of withdrawals">>,
        <<"operationId">> => <<"listWithdrawals">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the owner's identity">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the funds withdrawal">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"destinationID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Identifier of the destination">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        }, #{
          <<"name">> => <<"createdAtFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date range start">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"createdAtTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Creation date range end">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"amountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"amountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Amount of monetary funds in minor units">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Search result">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_7">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Create withdrawal">>,
        <<"operationId">> => <<"createWithdrawal">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"withdrawal">>,
          <<"description">> => <<"Withdrawal data">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Withdrawal started">>,
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI of started withdrawal">>
              }
            },
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Withdrawal">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"409">> => #{
            <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid data for withdrawal">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/withdrawals/{withdrawalID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Get withdrawal status">>,
        <<"operationId">> => <<"getWithdrawal">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the withdrawal">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Withdrawal found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Withdrawal">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/withdrawals/{withdrawalID}/events">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Request withdrawal events">>,
        <<"operationId">> => <<"pollWithdrawalEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the withdrawal">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"eventCursor">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"The identifier of the last known event.\n\nAll events that occurred _after_ the specified one will be included in the selection.\nIf this parameter is not specified, the selection will include events starting from the very first one.\n">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Events found">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/WithdrawalEvent">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/withdrawals/{withdrawalID}/events/{eventID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Get an event of withdrawal">>,
        <<"operationId">> => <<"getWithdrawalEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the withdrawal">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"eventID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Identifier of the identification procedure event.\n">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Event found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WithdrawalEvent">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Use [JWT](https://jwt.io) for call authentication. The corresponding key is passed in the header.\n```shell\n Authorization: Bearer {YOUR_API_KEY_JWT}\n```\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"Asset">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"The amount of money\n">>
    },
    <<"BankCardDestinationResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/DestinationResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      } ],
      <<"description">> => <<"Card">>
    },
    <<"BankCardReceiverResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/ReceiverResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"paymentSystem">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Payment system.\n\nThe set of systems available for making withdrawals can be found by calling the corresponding [operation](#operation/getWithdrawalMethods).\n">>,
            <<"readOnly">> => true
          }
        }
      } ],
      <<"description">> => <<"Card">>
    },
    <<"BankCardReceiverResourceParams">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/ReceiverResourceParams">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"BankCardSenderResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SenderResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"paymentSystem">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Payment system.\n\nThe set of systems available for making withdrawals can be found by calling the corresponding [operation](#operation/getWithdrawalMethods).\n">>,
            <<"readOnly">> => true
          }
        }
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"BankCardSenderResourceParams">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SenderResourceParams">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"authData">> ],
        <<"properties">> => #{
          <<"authData">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Authorization data received when saving the card">>,
            <<"maxLength">> => 1000
          }
        }
      } ],
      <<"description">> => <<"Card">>
    },
    <<"BrowserGetRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/BrowserRequest">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"uriTemplate">> ],
        <<"properties">> => #{
          <<"uriTemplate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"URL value template for browser navigation\n\nThe template is represented according to the standard\n[RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
          }
        }
      } ]
    },
    <<"BrowserPostRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/BrowserRequest">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"form">>, <<"uriTemplate">> ],
        <<"properties">> => #{
          <<"uriTemplate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"URL value template for form submission\n\nThe template is represented according to the standard\n[RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
          },
          <<"form">> => #{
            <<"$ref">> => <<"#/definitions/UserInteractionForm">>
          }
        }
      } ]
    },
    <<"BrowserRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"requestType">> ],
      <<"discriminator">> => <<"requestType">>,
      <<"properties">> => #{
        <<"requestType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of browser operation">>
        }
      }
    },
    <<"ContactInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"email">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"email">>,
          <<"description">> => <<"Email address">>,
          <<"maxLength">> => 100
        },
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"^\\+\\d{4,15}$">>,
          <<"description">> => <<"Mobile phone number with international prefix according to\n[E.164](https://en.wikipedia.org/wiki/E.164).\n">>
        }
      },
      <<"description">> => <<"Contact details">>
    },
    <<"ContinuationToken">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
    },
    <<"CryptoCurrency">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Cryptocurrency.\n\nThe set of cryptocurrencies available for withdrawals can be found out by calling the appropriate [operation](#operation/getWithdrawalMethods).\n">>,
      <<"example">> => <<"BTC">>
    },
    <<"CryptoWallet">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"zu3TcwGI71Bpaaw2XkLWZXlhMdn4zpVzMQ">>,
          <<"description">> => <<"Identifier (aka address) of a cryptocurrency wallet">>,
          <<"minLength">> => 16,
          <<"maxLength">> => 256
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"BTC">>,
          <<"description">> => <<"Cryptocurrency.\n\nThe set of cryptocurrencies available for withdrawals can be found out by calling the appropriate [operation](#operation/getWithdrawalMethods).\n">>
        }
      },
      <<"description">> => <<"Cryptocurrency wallet details">>
    },
    <<"CryptoWalletDestinationResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/DestinationResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ],
      <<"description">> => <<"Cryptocurrency funds">>
    },
    <<"Currency">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"exponent">>, <<"id">>, <<"name">>, <<"numericCode">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"numericCode">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"840">>,
          <<"description">> => <<"Digital currency code according to\n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm)\n">>,
          <<"pattern">> => <<"^[0-9]{3}$">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"United States Dollar">>,
          <<"description">> => <<"Human readable currency name\n">>
        },
        <<"sign">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"$">>,
          <<"description">> => <<"Currency unit sign\n">>
        },
        <<"exponent">> => #{
          <<"type">> => <<"integer">>,
          <<"example">> => 2,
          <<"description">> => <<"The number of acceptable decimal places in the amount of funds, \nin which the number of minor monetary units can be indicated\n">>,
          <<"minimum">> => 0
        }
      },
      <<"description">> => <<"Currency description">>,
      <<"example">> => #{
        <<"name">> => <<"United States Dollar">>,
        <<"sign">> => <<"$">>,
        <<"id">> => <<"USD">>,
        <<"numericCode">> => <<"840">>,
        <<"exponent">> => 2
      }
    },
    <<"CurrencyID">> => #{
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>,
      <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
      <<"example">> => <<"USD">>
    },
    <<"Deposit">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"body">>, <<"id">>, <<"source">>, <<"wallet">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"tZ0jUmlsV0">>,
            <<"description">> => <<"Deposit identifier">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Deposit start date and time">>,
            <<"readOnly">> => true
          },
          <<"wallet">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Identifier of the wallet">>
          },
          <<"source">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Funds source identifier">>
          },
          <<"body">> => #{
            <<"$ref">> => <<"#/definitions/Deposit_body">>
          },
          <<"fee">> => #{
            <<"$ref">> => <<"#/definitions/Deposit_fee">>
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/DepositStatus">>
      } ],
      <<"description">> => <<"Deposit data">>
    },
    <<"DepositAdjustment">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"tZ0jUmlsV0">>,
            <<"description">> => <<"Deposit adjustment identifier">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Date and time the adjustment was started">>,
            <<"readOnly">> => true
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/DepositAdjustmentStatus">>
      } ],
      <<"description">> => <<"Deposit adjustment data">>
    },
    <<"DepositAdjustmentFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Adjustment error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      }
    },
    <<"DepositAdjustmentID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Deposit adjustment identifier">>,
      <<"example">> => <<"tZ0jUmlsV0">>
    },
    <<"DepositAdjustmentStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Deposit adjustment status.\n\n| Meaning     | Explanation                                             |\n| ----------- | ------------------------------------------------------- |\n| `Pending`   | Adjustment in progress                                  |\n| `Succeeded` | Adjustment completed successfully                       |\n| `Failed`    | Adjustment failed                                       |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/DepositAdjustmentStatus_failure">>
        }
      }
    },
    <<"DepositFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Deposit error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      }
    },
    <<"DepositID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Deposit identifier">>,
      <<"example">> => <<"tZ0jUmlsV0">>
    },
    <<"DepositRevert">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"body">>, <<"source">>, <<"wallet">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Deposit revert identifier">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Date and time of revert start">>,
            <<"readOnly">> => true
          },
          <<"wallet">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Identifier of the wallet">>
          },
          <<"source">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Funds source identifier">>
          },
          <<"body">> => #{
            <<"$ref">> => <<"#/definitions/DepositRevert_body">>
          },
          <<"reason">> => #{
            <<"type">> => <<"string">>
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/DepositRevertStatus">>
      } ],
      <<"description">> => <<"Deposit revert data">>
    },
    <<"DepositRevertFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Deposit revert error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      }
    },
    <<"DepositRevertID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Deposit revert identifier">>,
      <<"example">> => <<"10068321">>
    },
    <<"DepositRevertStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Deposit revert status.\n\n| Meaning     | Explanation                                             |\n| ----------- | ------------------------------------------------------- |\n| `Pending`   | Deposit revert in progress                              |\n| `Succeeded` | Deposit revert completed successfully                   |\n| `Failed`    | Deposit revert failed                                   |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/DepositRevertStatus_failure">>
        }
      }
    },
    <<"DepositStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Status of deposit.\n\n| Meaning     | Explanation                                      |\n| ----------- | ------------------------------------------------ |\n| `Pending`   | Deposit in progress                              |\n| `Succeeded` | Deposit of funds made successfully               |\n| `Failed`    | Deposit of funds ended in failure                |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/DepositStatus_failure">>
        }
      }
    },
    <<"Destination">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"currency">>, <<"identity">>, <<"name">>, <<"resource">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Destination identifier">>,
            <<"readOnly">> => true
          },
          <<"name">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"Squarey plastic thingy">>,
            <<"description">> => <<"A human-readable name for the destination by which it is easily recognizable\n">>
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Date and time of creation of the destination of the funds">>,
            <<"readOnly">> => true
          },
          <<"isBlocked">> => #{
            <<"type">> => <<"boolean">>,
            <<"example">> => false,
            <<"description">> => <<"Is the destination blocked?">>,
            <<"readOnly">> => true
          },
          <<"identity">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"Identifier of wallet owner">>
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"USD">>,
            <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"resource">> => #{
            <<"$ref">> => <<"#/definitions/DestinationResource">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"example">> => #{
              <<"color_hint">> => <<"olive-green">>
            },
            <<"description">> => <<"Some non-transparent for system set of data associated with this destination\n">>,
            <<"properties">> => #{ }
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/DestinationStatus">>
      } ],
      <<"description">> => <<"Destination data">>
    },
    <<"DestinationGrantRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"validUntil">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Token granting the permission to control the withdrawals">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"The date and time by which the granted right is valid\n">>
        }
      },
      <<"description">> => <<"Request for the permission to control the withdrawals to the destination">>,
      <<"example">> => #{
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"token">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
      }
    },
    <<"DestinationID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Destination identifier">>,
      <<"example">> => <<"107498">>
    },
    <<"DestinationResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The resource type of the destination.\n\nSee [Vality Withdrawal Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardDestinationResource">>, <<"CryptoWalletDestinationResource">>, <<"DigitalWalletDestinationResource">> ]
        }
      },
      <<"description">> => <<"Destination resource used to make withdrawals">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"DestinationStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Authorized">>,
          <<"description">> => <<"The status of the destination.\n\n| Meaning        | Explanation                                   |\n| -------------- | --------------------------------------------- |\n| `Unauthorized` | Not authorized by the owner to withdraw funds |\n| `Authorized`   | Authorized by the owner to withdraw funds     |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Unauthorized">>, <<"Authorized">> ]
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"> If `status` == `Authorized`\n\nDate and time until which authorization is valid\n">>,
          <<"readOnly">> => true
        }
      }
    },
    <<"DestinationsTopic">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WebhookScope">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"eventTypes">> ],
        <<"properties">> => #{
          <<"eventTypes">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Set of event types of the destination, which should be notified">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"enum">> => [ <<"DestinationCreated">>, <<"DestinationUnauthorized">>, <<"DestinationAuthorized">> ]
            }
          }
        }
      } ],
      <<"description">> => <<"A coverage area that includes events by asset destinations within a particular wallet\n">>
    },
    <<"DigitalWallet">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"provider">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"zu3TcwGI71Bpaaw2XkLWZXlhMdn4zpVzMQ">>,
          <<"description">> => <<"Digital wallet ID">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 100
        },
        <<"accountName">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Name of the owner of the personal account to which the withdrawal will be made">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 1000
        },
        <<"provider">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Paypal">>,
          <<"description">> => <<"Digital wallet provider.\n\nThe set of providers available for making withdrawals can be found by calling\ncorresponding [operation](#operation/getWithdrawalMethods).\n">>
        },
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<" eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c">>,
          <<"description">> => <<"A string containing authorization data for transactions on this digital wallet">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        }
      },
      <<"description">> => <<"Digital wallet data">>
    },
    <<"DigitalWalletDestinationResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/DestinationResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/DigitalWallet">>
      } ],
      <<"description">> => <<"Digital wallet">>
    },
    <<"DigitalWalletProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Digital wallet provider.\n\nThe set of providers available for making withdrawals can be found by calling\ncorresponding [operation](#operation/getWithdrawalMethods).\n">>,
      <<"example">> => <<"Paypal">>
    },
    <<"InvalidOperationParameters">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"No such identity challenge type: fms.\n">>
        }
      },
      <<"description">> => <<"Invalid input data for operation">>
    },
    <<"ExternalID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>,
      <<"example">> => <<"10036274">>
    },
    <<"FileDownload">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"expiresAt">>, <<"url">> ],
      <<"properties">> => #{
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"URL of the file">>
        },
        <<"expiresAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"The date and time by which the link will be valid">>
        }
      },
      <<"example">> => #{
        <<"url">> => <<"url">>,
        <<"expiresAt">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"GenericProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Withdrawal service provider.\n\nThe set of providers available for making withdrawals can be found by calling\ncorresponding [operation](#operation/getWithdrawalMethods).\n">>,
      <<"example">> => <<"YourBankName">>
    },
    <<"GrantToken">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 4000,
      <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
    },
    <<"Identity">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"name">>, <<"provider">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Identifier of wallet owner">>,
          <<"readOnly">> => true
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Keyn Fawkes">>,
          <<"description">> => <<"Human-readable name of the owner's identity, by which he can be easily identified\n">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time the owner identity was created">>,
          <<"readOnly">> => true
        },
        <<"provider">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"serviceprovider">>,
          <<"description">> => <<"Identifier of the service provider">>
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"example">> => false,
          <<"description">> => <<"Is the owner's identity blocked?">>,
          <<"readOnly">> => true
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"example">> => #{
            <<"lkDisplayName">> => <<"James Smith">>
          },
          <<"description">> => <<"Some non-transparent for system set of data associated with this identity\n">>,
          <<"properties">> => #{ }
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
        },
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The participant's unique identifier within the system.">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        }
      },
      <<"description">> => <<"Data of the wallet owner">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"metadata">> => #{
          <<"lkDisplayName">> => <<"James Smith">>
        },
        <<"provider">> => <<"serviceprovider">>,
        <<"name">> => <<"Keyn Fawkes">>,
        <<"isBlocked">> => false,
        <<"externalID">> => <<"10036274">>,
        <<"id">> => <<"10036274">>,
        <<"partyID">> => <<"partyID">>
      }
    },
    <<"IdentityID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Identifier of wallet owner">>,
      <<"example">> => <<"10036274">>
    },
    <<"BadRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"errorType">> ],
      <<"properties">> => #{
        <<"errorType">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"NotFound">>,
          <<"description">> => <<"Error type">>,
          <<"enum">> => [ <<"SchemaViolated">>, <<"NotFound">>, <<"WrongType">>, <<"NotInRange">>, <<"WrongSize">>, <<"WrongLength">>, <<"WrongArray">>, <<"NoMatch">>, <<"InvalidResourceToken">>, <<"InvalidToken">> ]
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"X-Request-ID">>,
          <<"description">> => <<"Name or identifier of message element containing invalid data">>
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Required parameter was not sent">>,
          <<"description">> => <<"Explanation of why the data is invalid">>
        }
      }
    },
    <<"ConflictRequest">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"The passed value of `externalID` for which a request parameter conflict was detected">>
        },
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Identifier of the entity, created by a previous query with the specified `externalID'">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Human-readable description of the error">>
        }
      }
    },
    <<"BankCardPaymentSystem">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Payment system.\n\nThe set of systems available for making withdrawals can be found by calling the corresponding [operation](#operation/getWithdrawalMethods).\n">>
    },
    <<"SecuredBankCard">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"token">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"zu3TcwGI71Bpaaw2XkLWZXlhMdn4zpVzMQg9xMkh">>,
          <<"description">> => <<"Token identifying the original card data">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 1000
        },
        <<"bin">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"424242">>,
          <<"description">> => <<"[Identification number][1] of the card issuing bank\n\n[1]: https://en.wikipedia.org/wiki/Payment_card_number#Issuer_identification_number_(IIN)\n">>,
          <<"readOnly">> => true,
          <<"pattern">> => <<"^\\d{6,8}$">>
        },
        <<"lastDigits">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"4242">>,
          <<"description">> => <<"Card last digits">>,
          <<"readOnly">> => true,
          <<"pattern">> => <<"^\\d{2,4}$">>
        }
      },
      <<"description">> => <<"Secure bank card details">>
    },
    <<"PartyID">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"The participant's unique identifier within the system.">>
    },
    <<"Provider">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">>, <<"residences">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"serviceprovider">>,
          <<"description">> => <<"Identifier of the service provider">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"SERVICE PROVIDER LLC">>,
          <<"description">> => <<"Human-readable name of the service provider\n">>
        },
        <<"residences">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Residences in which the provider can service\n">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"RUS">>,
            <<"description">> => <<"Residence symbol code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        }
      },
      <<"description">> => <<"Service provider data">>,
      <<"example">> => #{
        <<"name">> => <<"SERVICE PROVIDER LLC">>,
        <<"id">> => <<"serviceprovider">>,
        <<"residences">> => [ <<"RUS">>, <<"RUS">> ]
      }
    },
    <<"ProviderID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Identifier of the service provider">>,
      <<"example">> => <<"serviceprovider">>
    },
    <<"QuoteParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"identityID">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/SenderResource">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/ReceiverResource">>
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Identifier of wallet owner">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/QuoteParameters_body">>
        }
      },
      <<"description">> => <<"Quote request parameters">>
    },
    <<"ReceiverResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The resource type of the receiver of the funds.\n\nSee [Vality Withdrawal Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardReceiverResource">> ]
        }
      },
      <<"description">> => <<"The beneficiary's resource used to make the transfers">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"ReceiverResourceParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The resource type of the receiver.\n\nSee [Vality Withdrawal Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardReceiverResourceParams">> ]
        }
      },
      <<"description">> => <<"Receiver resource parameters">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"Redirect">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"request">> ],
        <<"properties">> => #{
          <<"request">> => #{
            <<"$ref">> => <<"#/definitions/BrowserRequest">>
          }
        }
      } ]
    },
    <<"Report">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"createdAt">>, <<"files">>, <<"fromTime">>, <<"id">>, <<"status">>, <<"toTime">>, <<"type">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Report identifier">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time of creation">>
        },
        <<"fromTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time of the start of the period">>
        },
        <<"toTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time of the end of period">>
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Report generation status">>,
          <<"enum">> => [ <<"pending">>, <<"created">>, <<"canceled">> ]
        },
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Report type">>,
          <<"enum">> => [ <<"withdrawalRegistry">> ]
        },
        <<"files">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Report_files">>
          }
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"files">> => [ #{
          <<"id">> => <<"id">>
        }, #{
          <<"id">> => <<"id">>
        } ],
        <<"id">> => 0,
        <<"type">> => <<"withdrawalRegistry">>,
        <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"status">> => <<"pending">>
      }
    },
    <<"ReportParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"fromTime">>, <<"reportType">>, <<"toTime">> ],
      <<"properties">> => #{
        <<"reportType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of report">>,
          <<"enum">> => [ <<"withdrawalRegistry">> ]
        },
        <<"fromTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Start of the time period">>
        },
        <<"toTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"End of the time period">>
        }
      },
      <<"example">> => #{
        <<"reportType">> => <<"withdrawalRegistry">>,
        <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"Residence">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUS">>,
          <<"description">> => <<"Residence symbol code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"The United States of America">>,
          <<"description">> => <<"Human-readable name of the region of residence\n">>
        },
        <<"flag">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"🇺🇸">>,
          <<"description">> => <<"Residence region flag\n">>
        }
      },
      <<"description">> => <<"Description of the region of residence">>,
      <<"example">> => #{
        <<"flag">> => <<"🇺🇸">>,
        <<"name">> => <<"The United States of America">>,
        <<"id">> => <<"RUS">>
      }
    },
    <<"ResidenceID">> => #{
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>,
      <<"description">> => <<"Residence symbol code by standard [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
      <<"example">> => <<"RUS">>
    },
    <<"SenderResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The resource type of the sender of the funds.\n\nSee [Vality Withdrawal Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardSenderResource">> ]
        }
      },
      <<"description">> => <<"The sender resource used to make transfers">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"SenderResourceParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The resource type of the sender of the funds.\n\nSee [Vality Withdrawal Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardSenderResourceParams">> ]
        }
      },
      <<"description">> => <<"Fund sender resource settings">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"SourceID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Funds source identifier">>,
      <<"example">> => <<"107498">>
    },
    <<"SubFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Details of the error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"Detailed description of the error\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>
      }
    },
    <<"UserInteraction">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"interactionType">> ],
      <<"discriminator">> => <<"interactionType">>,
      <<"properties">> => #{
        <<"interactionType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of interaction with the user">>
        }
      }
    },
    <<"UserInteractionChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changeType">> ],
      <<"discriminator">> => <<"changeType">>,
      <<"properties">> => #{
        <<"changeType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Type of change in user interaction.">>,
          <<"enum">> => [ <<"UserInteractionCreated">>, <<"UserInteractionFinished">> ]
        }
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"UserInteractionCreated">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteractionChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"userInteraction">> ],
        <<"properties">> => #{
          <<"userInteraction">> => #{
            <<"$ref">> => <<"#/definitions/UserInteraction">>
          }
        }
      } ]
    },
    <<"UserInteractionFinished">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteractionChange">>
      }, #{ } ]
    },
    <<"UserInteractionForm">> => #{
      <<"type">> => <<"array">>,
      <<"description">> => <<"Browser submission form">>,
      <<"items">> => #{
        <<"$ref">> => <<"#/definitions/UserInteractionForm_inner">>
      }
    },
    <<"W2WTransfer">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"createdAt">>, <<"id">>, <<"receiver">>, <<"sender">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10a0b68D3E21">>,
          <<"description">> => <<"Transfer identifier">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time of creation">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/QuoteParameters_body">>
        },
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"status">> => #{
          <<"$ref">> => <<"#/definitions/W2WTransferStatus">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
        }
      },
      <<"description">> => <<"Transfer data">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"receiver">> => <<"10068321">>,
        <<"sender">> => <<"10068321">>,
        <<"externalID">> => <<"10036274">>,
        <<"id">> => <<"10a0b68D3E21">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        },
        <<"status">> => #{
          <<"failure">> => #{
            <<"code">> => <<"code">>,
            <<"subError">> => #{
              <<"code">> => <<"code">>
            }
          },
          <<"status">> => <<"Pending">>
        }
      }
    },
    <<"W2WTransferFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Main error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"[Error occurred during the transfer process](#tag/Error-Codes)\n">>
    },
    <<"W2WTransferID">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"Transfer identifier">>,
      <<"example">> => <<"10a0b68D3E21">>
    },
    <<"W2WTransferParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/W2WTransferParameters_body">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
        }
      },
      <<"description">> => <<"Transfer creation options">>,
      <<"example">> => #{
        <<"receiver">> => <<"10068321">>,
        <<"sender">> => <<"10068321">>,
        <<"externalID">> => <<"10036274">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        }
      }
    },
    <<"W2WTransferStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The status of the money transfer.\n\n| Meaning     | Explanation                          |\n| ----------- | ------------------------------------ |\n| `Pending`   | Transfer in progress              |\n| `Succeeded` | Fund transfer completed successfully |\n| `Failed`    | Fund transfer failed                 |\n">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/W2WTransferStatus_failure">>
        }
      },
      <<"example">> => #{
        <<"failure">> => #{
          <<"code">> => <<"code">>,
          <<"subError">> => #{
            <<"code">> => <<"code">>
          }
        },
        <<"status">> => <<"Pending">>
      }
    },
    <<"Wallet">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"identity">>, <<"name">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10068321">>,
          <<"description">> => <<"Identifier of the wallet">>,
          <<"readOnly">> => true
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Worldwide PHP Awareness Initiative">>,
          <<"description">> => <<"Human-readable name of the wallet, by which it is easy to recognize">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time of wallet creation">>,
          <<"readOnly">> => true
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"example">> => false,
          <<"description">> => <<"Is the wallet blocked?">>,
          <<"readOnly">> => true
        },
        <<"identity">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Identifier of wallet owner">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"example">> => #{
            <<"client_locale">> => <<"en_US">>
          },
          <<"description">> => <<"Some non-transparent for system set of data associated with this wallet\n">>,
          <<"properties">> => #{ }
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
        }
      },
      <<"description">> => <<"Wallet details">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"metadata">> => #{
          <<"client_locale">> => <<"en_US">>
        },
        <<"identity">> => <<"10036274">>,
        <<"name">> => <<"Worldwide PHP Awareness Initiative">>,
        <<"isBlocked">> => false,
        <<"externalID">> => <<"10036274">>,
        <<"currency">> => <<"USD">>,
        <<"id">> => <<"10068321">>
      }
    },
    <<"WalletAccount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"available">>, <<"own">> ],
      <<"properties">> => #{
        <<"own">> => #{
          <<"$ref">> => <<"#/definitions/WalletAccount_own">>
        },
        <<"available">> => #{
          <<"$ref">> => <<"#/definitions/WalletAccount_available">>
        }
      },
      <<"description">> => <<"Wallet account status">>,
      <<"example">> => #{
        <<"own">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        },
        <<"available">> => <<"{\"amount\":1200000,\"currency\":\"USD\"}">>
      }
    },
    <<"WalletGrantRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"asset">>, <<"validUntil">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"A token that gives the permission to one-time management of funds on the wallet">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"asset">> => #{
          <<"$ref">> => <<"#/definitions/WalletGrantRequest_asset">>
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time until which the granted right is valid\n">>
        }
      },
      <<"description">> => <<"Request for a one-time permission to manage funds on the wallet">>,
      <<"example">> => #{
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"asset">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        },
        <<"token">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
      }
    },
    <<"WalletID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Identifier of the wallet">>,
      <<"example">> => <<"10068321">>
    },
    <<"WalletName">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Human-readable name of the wallet, by which it is easy to recognize">>,
      <<"example">> => <<"Worldwide PHP Awareness Initiative">>
    },
    <<"Webhook">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"identityID">>, <<"scope">>, <<"url">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Identifier of the webhook\n">>,
          <<"readOnly">> => true
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Identifier of wallet owner">>
        },
        <<"active">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Is notification delivery currently enabled?\n">>,
          <<"readOnly">> => true
        },
        <<"scope">> => #{
          <<"$ref">> => <<"#/definitions/WebhookScope">>
        },
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri">>,
          <<"description">> => <<"The URL that will receive notifications of events that have occurred\n">>,
          <<"maxLength">> => 1000
        },
        <<"publicKey">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"hexadecimal">>,
          <<"example">> => <<"MIGJAoGBAM1fmNUvezts3yglTdhXuqG7OhHxQtDFA+Ss//YuUGjw5ossDbEMoS+SIFuYZ/UL9Xg0rEHNRSbmf48OK+mz0FobEtbji8MADayzGfFopXsfRFa7MVy3Uhu5jBDpLsN3DyJapAkK0TAYINlZXxVjDwxRNheTvC+xub5WNdiwc28fAgMBAAE=">>,
          <<"description">> => <<"The content of the public key used to check the authoritativeness of\nnotifications coming to `url`\n">>,
          <<"readOnly">> => true
        }
      },
      <<"example">> => #{
        <<"identityID">> => <<"10036274">>,
        <<"scope">> => #{
          <<"topic">> => <<"WithdrawalsTopic">>
        },
        <<"active">> => true,
        <<"id">> => <<"id">>,
        <<"publicKey">> => <<"MIGJAoGBAM1fmNUvezts3yglTdhXuqG7OhHxQtDFA+Ss//YuUGjw5ossDbEMoS+SIFuYZ/UL9Xg0rEHNRSbmf48OK+mz0FobEtbji8MADayzGfFopXsfRFa7MVy3Uhu5jBDpLsN3DyJapAkK0TAYINlZXxVjDwxRNheTvC+xub5WNdiwc28fAgMBAAE=">>,
        <<"url">> => <<"http://example.com/aeiou">>
      }
    },
    <<"WebhookScope">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"topic">> ],
      <<"discriminator">> => <<"topic">>,
      <<"properties">> => #{
        <<"topic">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Subject of notifications">>,
          <<"enum">> => [ <<"WithdrawalsTopic">>, <<"DestinationsTopic">> ]
        }
      },
      <<"description">> => <<"The scope of a webhook, limiting the set of event types,\nfor which the notifications should be sent\n">>,
      <<"example">> => #{
        <<"topic">> => <<"WithdrawalsTopic">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"Withdrawal">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"body">>, <<"destination">>, <<"wallet">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"tZ0jUmlsV0">>,
            <<"description">> => <<"Identifier of funds withdrawal">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Date and time the withdrawal started">>,
            <<"readOnly">> => true
          },
          <<"wallet">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Identifier of the wallet">>
          },
          <<"destination">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Destination identifier">>
          },
          <<"body">> => #{
            <<"$ref">> => <<"#/definitions/Withdrawal_body">>
          },
          <<"fee">> => #{
            <<"$ref">> => <<"#/definitions/Deposit_fee">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"example">> => #{
              <<"notify_email">> => <<"iliketrains@example.com">>
            },
            <<"description">> => <<"Some non-transparent for system set of data associated with this withdrawal\n">>,
            <<"properties">> => #{ }
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/WithdrawalStatus">>
      } ],
      <<"description">> => <<"Funds withdrawal data">>
    },
    <<"WithdrawalEvent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changes">>, <<"eventID">>, <<"occuredAt">> ],
      <<"properties">> => #{
        <<"eventID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"example">> => 42,
          <<"description">> => <<"Identifier of the funds withdrawal event">>
        },
        <<"occuredAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time the event occurrence">>
        },
        <<"changes">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalEventChange">>
          }
        }
      },
      <<"description">> => <<"An event that occurred during the funds withdrawal process\n">>,
      <<"example">> => #{
        <<"eventID">> => 42,
        <<"occuredAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"changes">> => [ #{
          <<"type">> => <<"WithdrawalStatusChanged">>
        }, #{
          <<"type">> => <<"WithdrawalStatusChanged">>
        } ]
      }
    },
    <<"WithdrawalEventChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The type of change that occurred">>,
          <<"enum">> => [ <<"WithdrawalStatusChanged">> ]
        }
      },
      <<"description">> => <<"Change that occurred in the funds withdrawal process\n">>,
      <<"example">> => #{
        <<"type">> => <<"WithdrawalStatusChanged">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"WithdrawalFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Withdrawal error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      }
    },
    <<"WithdrawalID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Identifier of funds withdrawal">>,
      <<"example">> => <<"tZ0jUmlsV0">>
    },
    <<"WithdrawalMethod">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"method">> ],
      <<"discriminator">> => <<"method">>,
      <<"properties">> => #{
        <<"method">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Withdrawal method">>,
          <<"enum">> => [ <<"WithdrawalMethodBankCard">>, <<"WithdrawalMethodDigitalWallet">>, <<"WithdrawalMethodGeneric">> ]
        }
      },
      <<"example">> => #{
        <<"method">> => <<"WithdrawalMethodBankCard">>
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"WithdrawalMethodBankCard">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WithdrawalMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"paymentSystems">> ],
        <<"properties">> => #{
          <<"paymentSystems">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of payment systems">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"description">> => <<"Payment system.\n\nThe set of systems available for making withdrawals can be found by calling the corresponding [operation](#operation/getWithdrawalMethods).\n">>
            }
          }
        }
      } ]
    },
    <<"WithdrawalMethodDigitalWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WithdrawalMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"providers">> ],
        <<"properties">> => #{
          <<"providers">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of digital wallet providers">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"example">> => <<"Paypal">>,
              <<"description">> => <<"Digital wallet provider.\n\nThe set of providers available for making withdrawals can be found by calling\ncorresponding [operation](#operation/getWithdrawalMethods).\n">>
            }
          }
        }
      } ]
    },
    <<"WithdrawalMethodGeneric">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WithdrawalMethod">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"providers">> ],
        <<"properties">> => #{
          <<"providers">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"List of withdrawal service providers">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"example">> => <<"YourBankName">>,
              <<"description">> => <<"Withdrawal service provider.\n\nThe set of providers available for making withdrawals can be found by calling\ncorresponding [operation](#operation/getWithdrawalMethods).\n">>
            }
          }
        }
      } ]
    },
    <<"WithdrawalParameters">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Withdrawal">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"walletGrant">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
            <<"description">> => <<"A token that gives the right to withdraw from the wallet to pay for the withdrawal.\n\nMust be provided if withdrawal is made at the expense of _foreign_\nwallet. The owner of said wallet can\n[issue this right](#operation/issueWalletGrant).\n">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 4000
          },
          <<"destinationGrant">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
            <<"description">> => <<"A token that gives the right to withdraw.\n\nMust be provided if the withdrawal is made through a _foreign_ recipient of\nfunds. The owner of the specified recipient can\n[issue this right](#operation/issueDestinationGrant).\n">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 4000
          },
          <<"quoteToken">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
            <<"description">> => <<"Quote at which funds should be withdrawn.\n\nMust be [obtained](#operation/createQuote)\nin advance for each individual withdrawal operation with conversion.\n">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 4000
          }
        }
      } ],
      <<"description">> => <<"Options of generated withdrawal">>
    },
    <<"WithdrawalQuote">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cashFrom">>, <<"cashTo">>, <<"createdAt">>, <<"expiresOn">>, <<"quoteToken">> ],
      <<"properties">> => #{
        <<"cashFrom">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalQuote_cashFrom">>
        },
        <<"cashTo">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalQuote_cashTo">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Date and time the quote was received">>,
          <<"readOnly">> => true
        },
        <<"expiresOn">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Quote expiration date and time">>,
          <<"readOnly">> => true
        },
        <<"quoteToken">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Quote at which funds should be withdrawn. Must be provided when creating withdrawal with conversion\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        }
      },
      <<"description">> => <<"Quote data for withdrawal">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"cashTo">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        },
        <<"cashFrom">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        },
        <<"quoteToken">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
        <<"expiresOn">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"WithdrawalQuoteParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cash">>, <<"currencyFrom">>, <<"currencyTo">>, <<"walletID">> ],
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"The unique identifier of the entity on your side.\n\nWhen specified, will be used to ensure idempotent processing of the operation.\n">>
        },
        <<"walletID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10068321">>,
          <<"description">> => <<"Identifier of the wallet">>
        },
        <<"destinationID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"107498">>,
          <<"description">> => <<"Destination identifier">>
        },
        <<"currencyFrom">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Source currency code">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"currencyTo">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Target currency code">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"cash">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalQuoteParams_cash">>
        },
        <<"walletGrant">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"A token that gives the right to withdraw from the wallet to pay for the withdrawal. It is necessary to provide if the withdrawal is made at the expense of the funds of a _foreign_ wallet. The owner of the specified wallet can [issue this right](#operation/issueWalletGrant)\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"destinationGrant">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"A token that gives the right to withdraw. Must be provided if the withdrawal is made through a _foreign_ fund recipient. The owner of the specified recipient can [grant this right](#operation/issueDestinationGrant)\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        }
      },
      <<"description">> => <<"Quote parameters for withdrawal">>,
      <<"example">> => #{
        <<"walletID">> => <<"10068321">>,
        <<"walletGrant">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
        <<"externalID">> => <<"10036274">>,
        <<"destinationGrant">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
        <<"destinationID">> => <<"107498">>,
        <<"currencyTo">> => <<"USD">>,
        <<"cash">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"USD">>
        },
        <<"currencyFrom">> => <<"USD">>
      }
    },
    <<"WithdrawalStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Withdrawal status.\n\n| Meaning     | Explanation                          |\n| ----------- | ------------------------------------ |\n| `Pending`   | Withdrawal in progress               |\n| `Succeeded` | Withdrawal completed successfully    |\n| `Failed`    | Withdrawal failed                    |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalStatus_failure">>
        }
      }
    },
    <<"WithdrawalStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WithdrawalEventChange">>
      }, #{
        <<"$ref">> => <<"#/definitions/WithdrawalStatus">>
      } ],
      <<"description">> => <<"Change of withdrawal status">>
    },
    <<"WithdrawalsTopic">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WebhookScope">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"eventTypes">> ],
        <<"properties">> => #{
          <<"walletID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Identifier of the wallet">>
          },
          <<"eventTypes">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Set of withdrawal event types to be notified about">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"enum">> => [ <<"WithdrawalStarted">>, <<"WithdrawalSucceeded">>, <<"WithdrawalFailed">> ]
            }
          }
        }
      } ],
      <<"description">> => <<"Scope that includes withdrawal events within a specific wallet\n">>
    },
    <<"inline_response_200">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Found adjustments">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/DepositAdjustment">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_1">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Found reverts">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/DepositRevert">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_2">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Found deposits">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Deposit">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_3">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Destinations found">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Destination">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_4">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Identities found">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Identity">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{
            <<"lkDisplayName">> => <<"James Smith">>
          },
          <<"provider">> => <<"serviceprovider">>,
          <<"name">> => <<"Keyn Fawkes">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"id">> => <<"10036274">>,
          <<"partyID">> => <<"partyID">>
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{
            <<"lkDisplayName">> => <<"James Smith">>
          },
          <<"provider">> => <<"serviceprovider">>,
          <<"name">> => <<"Keyn Fawkes">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"id">> => <<"10036274">>,
          <<"partyID">> => <<"partyID">>
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_5">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"methods">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalMethod">>
          }
        }
      },
      <<"example">> => #{
        <<"methods">> => [ #{
          <<"method">> => <<"WithdrawalMethodBankCard">>
        }, #{
          <<"method">> => <<"WithdrawalMethodBankCard">>
        } ]
      }
    },
    <<"inline_response_200_6">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Wallets found">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Wallet">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{
            <<"client_locale">> => <<"en_US">>
          },
          <<"identity">> => <<"10036274">>,
          <<"name">> => <<"Worldwide PHP Awareness Initiative">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"currency">> => <<"USD">>,
          <<"id">> => <<"10068321">>
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{
            <<"client_locale">> => <<"en_US">>
          },
          <<"identity">> => <<"10036274">>,
          <<"name">> => <<"Worldwide PHP Awareness Initiative">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"currency">> => <<"USD">>,
          <<"id">> => <<"10068321">>
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_7">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Withdrawals found">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Withdrawal">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"Deposit_body">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"The amount of funds received">>
    },
    <<"Deposit_fee">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Fee amount">>
    },
    <<"DepositAdjustmentStatus_failure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Adjustment error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"> If `status` == `Failed`\n\nExplanation of the reason for failure\n">>
    },
    <<"DepositRevert_body">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Amount of funds">>
    },
    <<"DepositRevertStatus_failure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Deposit revert error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"> If `status` == `Failed`\n\nExplanation of the reason for failure\n">>
    },
    <<"DepositStatus_failure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Deposit error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"> If `status` == `Failed`\n\nExplanation of the reason for failure\n">>
    },
    <<"QuoteParameters_body">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Transaction amount">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"Report_files">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"File identifier">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        }
      },
      <<"example">> => #{
        <<"id">> => <<"id">>
      }
    },
    <<"UserInteractionForm_inner">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"key">>, <<"template">> ],
      <<"properties">> => #{
        <<"key">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The value of the key of the form element to be send by means of browser\n">>
        },
        <<"template">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"The template for the form element value\nThe template is presented according to the standard\n[RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
        }
      }
    },
    <<"W2WTransferParameters_body">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Transfer amount">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"W2WTransferStatus_failure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Main error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"[Error occurred during the transfer process](#tag/Error-Codes)\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>,
        <<"subError">> => #{
          <<"code">> => <<"code">>
        }
      }
    },
    <<"WalletAccount_own">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Own funds\n">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"WalletAccount_available">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Funds available for use. Usually equal to own funds\nminus the sum of all pending transactions\n">>,
      <<"example">> => <<"{\"amount\":1200000,\"currency\":\"USD\"}">>
    },
    <<"WalletGrantRequest_asset">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Amount of funds allowed for use">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"Withdrawal_body">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Amount of funds to be withdrawn">>
    },
    <<"WithdrawalQuote_cashFrom">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Amount of funds in source currency">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"WithdrawalQuote_cashTo">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Amount of funds in target currency">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"WithdrawalQuoteParams_cash">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"The amount of money in minor units, for example, in cents\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"USD">>,
          <<"description">> => <<"Currency character code according to \n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"The amount of funds for receiving a quote in one of the exchange currencies">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"USD">>
      }
    },
    <<"WithdrawalStatus_failure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Withdrawal error code">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"> If `status` == `Failed`\n\nExplaining the reason for failure\n">>
    }
  },
  <<"parameters">> => #{
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Unique identifier of the request to the system">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    },
    <<"providerID">> => #{
      <<"name">> => <<"providerID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of the provider">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"identityID">> => #{
      <<"name">> => <<"identityID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of the owner's identity">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"walletID">> => #{
      <<"name">> => <<"walletID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of the wallet">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"destinationID">> => #{
      <<"name">> => <<"destinationID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of the destination">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"withdrawalID">> => #{
      <<"name">> => <<"withdrawalID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of the withdrawal">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"externalID">> => #{
      <<"name">> => <<"externalID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"External identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>
    },
    <<"residence">> => #{
      <<"name">> => <<"residence">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"The residence within which the services are provided,\n[ISO 3166-1] country or region code (https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Za-z]{3}$">>
    },
    <<"amountFrom">> => #{
      <<"name">> => <<"amountFrom">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Amount of monetary funds in minor units">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"amountTo">> => #{
      <<"name">> => <<"amountTo">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Amount of monetary funds in minor units">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"currencyID">> => #{
      <<"name">> => <<"currencyID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Currency, character code according to [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>
    },
    <<"limit">> => #{
      <<"name">> => <<"limit">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Selection limit">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"maximum">> => 1000,
      <<"minimum">> => 1,
      <<"format">> => <<"int32">>
    },
    <<"eventCursor">> => #{
      <<"name">> => <<"eventCursor">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"The identifier of the last known event.\n\nAll events that occurred _after_ the specified one will be included in the selection.\nIf this parameter is not specified, the selection will include events starting from the very first one.\n">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int32">>
    },
    <<"eventID">> => #{
      <<"name">> => <<"eventID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of the identification procedure event.\n">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int32">>
    },
    <<"reportID">> => #{
      <<"name">> => <<"reportID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"The report identifier">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"fileID">> => #{
      <<"name">> => <<"fileID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"The file identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"fromTime">> => #{
      <<"name">> => <<"fromTime">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Start of the time period">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"format">> => <<"date-time">>
    },
    <<"toTime">> => #{
      <<"name">> => <<"toTime">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"End of the time period">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"format">> => <<"date-time">>
    },
    <<"deadline">> => #{
      <<"name">> => <<"X-Request-Deadline">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Maximum request processing time">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"webhookID">> => #{
      <<"name">> => <<"webhookID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Webhook identifier">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"queryIdentityID">> => #{
      <<"name">> => <<"identityID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Identifier of the owner's identity">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"w2wTransferID">> => #{
      <<"name">> => <<"w2wTransferID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Identifier of transfer">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"partyID">> => #{
      <<"name">> => <<"partyID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"The participant's unique identifier within the system.">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    }
  },
  <<"responses">> => #{
    <<"BadRequest">> => #{
      <<"description">> => <<"Invalid input data for operation">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/BadRequest">>
      }
    },
    <<"ConflictRequest">> => #{
      <<"description">> => <<"The passed value `externalID` has already been used by you with other query parameters">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/ConflictRequest">>
      }
    },
    <<"NotFound">> => #{
      <<"description">> => <<"The content you are looking for was not found">>
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Authorization error">>
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": \"personType\",
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {\"$ref\": \"#/definitions/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": \"dummyType\",
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(deep_get([?DEFINITIONS, Parent, <<"properties">>, Discr, <<"enum">>], Schema)).

deep_get([K], M) ->
    maps:get(K, M);
deep_get([K | Ks], M) ->
    deep_get(Ks, maps:get(K, M)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw())),
       ?MODULE:get()
    ).
-endif.
