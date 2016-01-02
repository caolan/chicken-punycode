# Punycode

A CHICKEN Scheme implementation of [RFC 3492][rfc3492]:
"Punycode: A Bootstring encoding of Unicode for Internationalized
Domain Names in Applications (IDNA)"

```scheme
(use punycode)

(punycode-encode "Bücher")
;; => "Bcher-kva"

(punycode-decode "Bcher-kva")
;; => "Bücher"
```

This implementation does not support producing mixed-case
annotations when encoding (which is not required by the RFC).

[rfc3492]: https://www.ietf.org/rfc/rfc3492.txt
