# Empayre Wallet API Specification

This is a set of patches / extensions over https://github.com/valitydev/swag-wallets.

Patches currently serve two purposes:
1. Prevent too much drift from the upstream.
2. Inject brand-specific documentation.
3. Extend specification in controlled points.

## Points of extensions

### Generic destination resources

This allows adding new destination resources which should be handled by _wapi_ in a generic manner.

Provided through vendor extension `x-vality-genericMethod`, which should be a JSON object. You can set it on a model discriminated against `DestinationResource`. When it's set on an object its `type` value will be used to identify this resource's [payment service](https://github.com/valitydev/fistful-proto/blob/eeff5ba9/proto/base.thrift#L167).

At the moment this object may contain following properties:

1. `schema` object, which should be a valid JSON schema.

    A subset of `DestinationResource` object strictly following this schema will be interpreted as _generic resource data_ and passed down as part of [`ResourceGeneric.generic.data`](https://github.com/valitydev/fistful-proto/blob/eeff5ba9/proto/base.thrift#L179) thrift model (which will eventually become [`GenericPaymentTool.data`](https://github.com/valitydev/damsel/blob/b25d3365/proto/domain.thrift#L1855)).

    When `id` schema property is defined, its value will be used to annotate _generic resource data_ data, namely through [MIME type](https://github.com/valitydev/fistful-proto/blob/eeff5ba9/proto/base.thrift#L174).
