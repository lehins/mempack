# Changelog for `mempack`

## 0.2.0.0

* Add an offset to the `buffer` function in order to support types that don't use the full
  underlying buffer.
* Add `Buffer` and `MemPack` instances for `PrimArray` and older definition of `ByteArray` from `primitive`.

## 0.1.2.0

* Fix 32-bit support

## 0.1.1.0

* Add helpers `packByteStringM`, `unpackByteStringM` and `unpackByteArrayLen`
* Add `MemPack` instance for lazy `ByteString`
* Add `MemPack` instance for `Text`
* Add `MemPack` instance for `Void`
* Add `packWithByteArray` and `packWithMutableByteArray`
* Fix infinite loop during list decoding when length encoded was negative

## 0.1.0.0

* Initial release
