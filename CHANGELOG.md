# Swag-wallets changelog

## Version 0.0.3 to 0.0.4 - 16.11.2021
---
### What's Deprecated(not supported yet)
---
Удалена процедура идентификации владельца кошелька.

`GET`  /identities/{identityID}/challenges
`POST` /identities/{identityID}/challenges
`GET`  /identities/{identityID}/challenges/{challengeID}
`GET`  /identities/{identityID}/challenges/{challengeID}/events
`GET`  /identities/{identityID}/challenges/{challengeID}/events/{eventID}

Удалена информация о классе владельца и уровне идентификации

`GET` /providers/{providerID}/identity-classes
`GET` /providers/{providerID}/identity-classes/{identityClassID}
`GET` /providers/{providerID}/identity-classes/{identityClassID}/levels
`GET` /providers/{providerID}/identity-classes/{identityClassID}/levels/{identityLevelID}

## Version 0.0.2 to 0.0.3 - 22.09.2021
---
### What's Deprecated(not supported yet)
---
`POST` /p2p/quotes          Расчет комиссии и фиксирование их в виде токена, который можно использовать при создании перевода
`POST` /p2p/transfers       Создание перевода
`GET`  /p2p/transfers/{p2pTransferID}         Получить перевод
`GET`  /p2p/transfers/{p2pTransferID}/events  Получить историю перевода в виде набора событий.

`POST` /p2p/transfer/templates  Создать шаблон перевода
`GET`  /p2p/transfer/templates/{p2pTransferTemplateID} Получить шаблон перевода по его идентификатору.
`POST` /p2p/transfer/templates/{p2pTransferTemplateID}/access-tokens Получить токен доступа для чтения шаблона и выписывание талонов на создание переводов по шаблону
`POST` /p2p/transfer/templates/{p2pTransferTemplateID}/block Заблокировать использование шаблона.
`POST` /p2p/transfer/templates/{p2pTransferTemplateID}/quotes Расчитать комиссии и зафиксировать их в виде токена
`POST` /p2p/transfer/templates/{p2pTransferTemplateID}/tickets Получить талон на одну попытку создания перевода по шаблону
`POST` /p2p/transfer/templates/{p2pTransferTemplateID}/transfers Создать новый перевод по шаблону.


## Version 0.0.1 to 0.0.2 - 26.11.2020
---
### What's New
---
`POST` /bank-cards Сохранить банковскую карту
    Return Type

        Insert validUntil //Дата и время, до наступления которых токен платёжного ресурса остается действительным


## Version 0.0.1 to 0.0.1 - 11.11.2020
---
### What's New
---

### What's Deprecated
---
### What's Changed
---
`POST` /identities Создать личность владельца
    Parameters

        Insert identity.partyID //Уникальный в рамках платформы идентификатор участника.
`GET` /identities/{identityID} Получить данные личности владельца
    Return Type

        Insert partyID //Уникальный в рамках платформы идентификатор участника.

