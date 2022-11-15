# Shamelessly stolen from Django's django.contrib.auth module to ensure
# compatibility with Django auth models.

import base64
import hashlib
import math
import secrets
from typing import Optional

RANDOM_STRING_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"


def force_bytes(s, encoding="utf-8", errors="strict"):
    """
    Similar to smart_bytes, except that lazy instances are resolved to
    strings, rather than kept as lazy objects.

    If strings_only is True, don't convert (some) non-string-like objects.
    """
    # Handle the common case first for performance reasons.
    if isinstance(s, bytes):
        if encoding == "utf-8":
            return s
        else:
            return s.decode("utf-8", errors).encode(encoding, errors)
    if isinstance(s, memoryview):
        return bytes(s)
    return str(s).encode(encoding, errors)


def pbkdf2(password, salt, iterations, dklen=0, digest=None):
    """Return the hash of password using pbkdf2."""
    if digest is None:
        digest = hashlib.sha256
    dklen = dklen or None
    password = force_bytes(password)
    salt = force_bytes(salt)
    return hashlib.pbkdf2_hmac(digest().name, password, salt, iterations, dklen)


def verify_password(password: str, encoded: str) -> bool:
    algorithm, iterations, salt, _hash = encoded.split("$", 3)
    assert algorithm == "pbkdf2_sha256"
    encoded_2 = encode_password(password, salt, int(iterations))
    return encoded == encoded_2


def encode_password(password: str, salt: Optional[str] = None, iterations=None) -> str:
    if salt is None:
        char_count = math.ceil(128 / math.log2(len(RANDOM_STRING_CHARS)))
        salt = "".join(secrets.choice(RANDOM_STRING_CHARS) for i in range(char_count))
    iterations = iterations or 390000
    hash = pbkdf2(password, salt, iterations, digest=hashlib.sha256)
    hash = base64.b64encode(hash).decode("ascii").strip()
    return "%s$%d$%s$%s" % ("pbkdf2_sha256", iterations, salt, hash)
