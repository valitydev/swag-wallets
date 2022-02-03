%% -*- mode: erlang -*-
-module(swag_server_payres_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_server_payres:object().
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
    <<"description">> => <<"\nVality Payment Resource API служит для токенизации чувствительных данных платёжных ресурсов пользователей.\n">>,
    <<"version">> => <<"0.1.0">>,
    <<"title">> => <<"Vality Payment Resource API">>,
    <<"termsOfService">> => <<"https://vality.dev/">>,
    <<"contact">> => #{
      <<"name">> => <<"Команда техподдержки">>,
      <<"url">> => <<"https://vality.dev/">>,
      <<"email">> => <<"support@vality.dev">>
    }
  },
  <<"host">> => <<"api.vality.dev">>,
  <<"basePath">> => <<"/payres/v0">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Payment Resources">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Платёжные ресурсы">>
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
        <<"tags">> => [ <<"Payment Resources">> ],
        <<"summary">> => <<"Сохранить банковскую карту">>,
        <<"operationId">> => <<"storeBankCard">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"bankCard">>,
          <<"description">> => <<"Данные банковской карты">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/BankCard">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Карта сохранена">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/StoreBankCardResponse">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Непригодные данные банковской карты">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidBankCard">>
            }
          }
        }
      }
    },
    <<"/bank-cards/{token}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Payment Resources">> ],
        <<"summary">> => <<"Получить данные банковской карты">>,
        <<"operationId">> => <<"getBankCard">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"token">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Данные банковской карты">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 1000,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Данные карты найдены">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/SecuredBankCard">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Для аутентификации вызовов мы используем [JWT](https://jwt.io). Cоответствующий ключ передается в заголовке.\n```shell\n Authorization: Bearer {TOKENIZATION|PRIVATE_JWT}\n```\n">>,
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
          <<"description">> => <<"Данные для авторизации платежа по карте">>,
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
          <<"description">> => <<"Банковская карта">>,
          <<"enum">> => [ <<"BankCard">> ]
        },
        <<"cardNumber">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"4242424242424242">>,
          <<"description">> => <<"Номер банковской карты">>,
          <<"pattern">> => <<"^\\d{12,19}$">>
        },
        <<"expDate">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"12/21">>,
          <<"description">> => <<"Срок действия банковской карты">>,
          <<"pattern">> => <<"^\\d{2}\\/(\\d{2}|\\d{4})$">>
        },
        <<"cardHolder">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"LEXA SVOTIN">>,
          <<"description">> => <<"Имя держателя карты">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 100,
          <<"pattern">> => <<"^[[:alpha:][:space:][:punct:]]+$">>
        },
        <<"cvv">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"321">>,
          <<"description">> => <<"Код верификации">>,
          <<"pattern">> => <<"^\\d{3,4}$">>
        }
      },
      <<"description">> => <<"Данные банковской карты">>,
      <<"example">> => #{
        <<"cvv">> => <<"321">>,
        <<"cardHolder">> => <<"LEXA SVOTIN">>,
        <<"type">> => <<"BankCard">>,
        <<"cardNumber">> => <<"4242424242424242">>,
        <<"expDate">> => <<"12/21">>
      }
    },
    <<"BankCardPaymentSystem">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Платежная система">>
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
          <<"description">> => <<"Тип ошибки в данных">>,
          <<"enum">> => [ <<"SchemaViolated">>, <<"NotFound">>, <<"WrongType">>, <<"NotInRange">>, <<"WrongSize">>, <<"WrongLength">>, <<"WrongArray">>, <<"NoMatch">>, <<"InvalidResourceToken">>, <<"InvalidToken">> ]
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"X-Request-ID">>,
          <<"description">> => <<"Имя или идентификатор элемента сообщения, содержащего недопустимые данные">>
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Required parameter was not sent">>,
          <<"description">> => <<"Пояснение, почему данные считаются недопустимыми">>
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
          <<"description">> => <<"Токен, идентифицирующий исходные данные карты">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 1000
        },
        <<"bin">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"424242">>,
          <<"description">> => <<"[Идентификационный номер][1] банка-эмитента карты\n\n[1]: https://en.wikipedia.org/wiki/Payment_card_number#Issuer_identification_number_(IIN)\n">>,
          <<"readOnly">> => true,
          <<"pattern">> => <<"^\\d{6,8}$">>
        },
        <<"lastDigits">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"4242">>,
          <<"description">> => <<"Последние цифры номера карты">>,
          <<"readOnly">> => true,
          <<"pattern">> => <<"^\\d{2,4}$">>
        }
      },
      <<"description">> => <<"Безопасные данные банковской карты">>,
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
          <<"description">> => <<"Дата и время, до наступления которых токен платёжного ресурса остается действительным">>,
          <<"readOnly">> => true
        }
      }
    }
  },
  <<"parameters">> => #{
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    }
  },
  <<"responses">> => #{
    <<"NotFound">> => #{
      <<"description">> => <<"Искомая сущность не найдена">>
    },
    <<"BadRequest">> => #{
      <<"description">> => <<"Недопустимые для операции входные данные">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/BadRequest">>
      }
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
