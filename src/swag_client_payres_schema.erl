%% -*- mode: erlang -*-
-module(swag_client_payres_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_client_payres:object().
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
    <<"description">> => <<"\nThe Vality Payment Resource API is used to tokenize sensitive data of users' payment resources.\n">>,
    <<"version">> => <<"0.1.0">>,
    <<"title">> => <<"Vality Payment Resource API">>,
    <<"termsOfService">> => <<"https://vality.dev/">>,
    <<"contact">> => #{
      <<"name">> => <<"Support Team">>,
      <<"url">> => <<"https://vality.dev/">>,
      <<"email">> => <<"support@vality.dev">>
    }
  },
  <<"host">> => <<"api.vality.dev">>,
  <<"basePath">> => <<"/payres/v0">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Payment Resources">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Payment resources">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/bank-cards">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawal Resources">> ],
        <<"summary">> => <<"Save card">>,
        <<"operationId">> => <<"storeBankCard">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"bankCard">>,
          <<"description">> => <<"Сard details">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/BankCard">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Card saved">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/StoreBankCardResponse">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization Error">>
          },
          <<"422">> => #{
            <<"description">> => <<"Invalid сard details">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidBankCard">>
            }
          }
        }
      }
    },
    <<"/bank-cards/{token}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawal Resources">> ],
        <<"summary">> => <<"Receive bank card details">>,
        <<"operationId">> => <<"getBankCard">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"token">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Bank card details">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 1000,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Card data found">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/SecuredBankCard">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization Error">>
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
      <<"description">> => <<"Use [JWT](https://jwt.io) for call authentication. The corresponding key is passed in the header.\n```shell\n Authorization: Bearer {TOKENIZATION|PRIVATE_JWT}\n```\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"AuthData">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"authData">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"GVpEyibo60jJwyoJqriv1nAjiwfvzC9KVqFFjhhUA">>,
          <<"description">> => <<"Data for card withdrawal authorization">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 1000
        }
      }
    },
    <<"BankCard">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cardNumber">> ],
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Card">>,
          <<"enum">> => [ <<"BankCard">> ]
        },
        <<"cardNumber">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"4242424242424242">>,
          <<"description">> => <<"Card number">>,
          <<"pattern">> => <<"^\\d{12,19}$">>
        },
        <<"expDate">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"12/21">>,
          <<"description">> => <<"Сard expiration date">>,
          <<"pattern">> => <<"^\\d{2}\\/(\\d{2}|\\d{4})$">>
        },
        <<"cardHolder">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"LEEROY JENKINS">>,
          <<"description">> => <<"Cardholder name">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 100,
          <<"pattern">> => <<"^[[:alpha:][:space:][:punct:]]+$">>
        },
        <<"cvv">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"321">>,
          <<"description">> => <<"Verification code">>,
          <<"pattern">> => <<"^\\d{3,4}$">>
        }
      },
      <<"description">> => <<"Bank card details">>,
      <<"example">> => #{
        <<"cvv">> => <<"321">>,
        <<"cardHolder">> => <<"LEEROY JENKINS">>,
        <<"type">> => <<"BankCard">>,
        <<"cardNumber">> => <<"4242424242424242">>,
        <<"expDate">> => <<"12/21">>
      }
    },
    <<"BankCardPaymentSystem">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Payment system.\n\nThe set of systems available for making withdrawals can be found by calling the corresponding [operation](#operation/getWithdrawalMethods).\n">>
    },
    <<"InvalidBankCard">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Card is expired.\n">>
        }
      }
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
      <<"description">> => <<"Secure bank card details">>,
      <<"example">> => #{
        <<"bin">> => <<"424242">>,
        <<"lastDigits">> => <<"4242">>,
        <<"token">> => <<"zu3TcwGI71Bpaaw2XkLWZXlhMdn4zpVzMQg9xMkh">>
      }
    },
    <<"StoreBankCardResponse">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"$ref">> => <<"#/definitions/AuthData">>
      } ]
    },
    <<"ValidUntil">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"The date and time by which the withdrawal resource token remains valid">>,
          <<"readOnly">> => true
        }
      }
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
    }
  },
  <<"responses">> => #{
    <<"NotFound">> => #{
      <<"description">> => <<"The content you are looking for was not found">>
    },
    <<"BadRequest">> => #{
      <<"description">> => <<"Invalid input data for operation">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/BadRequest">>
      }
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Authorization Error">>
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
