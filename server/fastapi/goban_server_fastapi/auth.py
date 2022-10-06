# Shamelessly stolen from Django's django.contrib.auth module to ensure
# compatibility with Django auth models.

import base64
import hashlib
import math
import os
import secrets
from datetime import datetime, timedelta, timezone
from typing import Optional

import jwt
from fastapi import Depends, HTTPException, Request
from starlette.authentication import (
    AuthCredentials,
    AuthenticationBackend,
    AuthenticationError,
    SimpleUser,
)

from goban_server_fastapi.models import DbClient, User
from goban_server_fastapi.settings import *

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


def get_random_string(length, allowed_chars=RANDOM_STRING_CHARS):
    """
    Return a securely generated random string.

    The bit length of the returned value can be calculated with the formula:
        log_2(len(allowed_chars)^length)

    For example, with default `allowed_chars` (26+26+10), this gives:
      * length: 12, bit length =~ 71 bits
      * length: 22, bit length =~ 131 bits
    """
    return "".join(secrets.choice(allowed_chars) for i in range(length))


def mask_hash(hash, show=6, char="*"):
    """
    Return the given hash, with only the first ``show`` number shown. The
    rest are masked with ``char`` for security reasons.
    """
    masked = hash[:show]
    masked += char * len(hash[show:])
    return masked


def pbkdf2(password, salt, iterations, dklen=0, digest=None):
    """Return the hash of password using pbkdf2."""
    if digest is None:
        digest = hashlib.sha256
    dklen = dklen or None
    password = force_bytes(password)
    salt = force_bytes(salt)
    return hashlib.pbkdf2_hmac(digest().name, password, salt, iterations, dklen)


def must_update_salt(salt, expected_entropy):
    # Each character in the salt provides log_2(len(alphabet)) bits of entropy.
    return len(salt) * math.log2(len(RANDOM_STRING_CHARS)) < expected_entropy


class PBKDF2PasswordHasher:
    """
    Secure password hashing using the PBKDF2 algorithm (recommended)

    Configured to use PBKDF2 + HMAC + SHA256.
    The result is a 64 byte binary string.  Iterations may be changed
    safely but you must rename the algorithm if you change SHA256.
    """

    algorithm = "pbkdf2_sha256"
    iterations = 390000
    digest = hashlib.sha256

    def salt(self):
        """
        Generate a cryptographically secure nonce salt in ASCII with an entropy
        of at least `salt_entropy` bits.
        """
        # Each character in the salt provides
        # log_2(len(alphabet)) bits of entropy.
        char_count = math.ceil(128 / math.log2(len(RANDOM_STRING_CHARS)))
        return get_random_string(char_count, allowed_chars=RANDOM_STRING_CHARS)

    def encode(self, password, salt, iterations=None):
        iterations = iterations or self.iterations
        hash = pbkdf2(password, salt, iterations, digest=self.digest)
        hash = base64.b64encode(hash).decode("ascii").strip()
        return "%s$%d$%s$%s" % (self.algorithm, iterations, salt, hash)

    def decode(self, encoded):
        algorithm, iterations, salt, hash = encoded.split("$", 3)
        assert algorithm == self.algorithm
        return {
            "algorithm": algorithm,
            "hash": hash,
            "iterations": int(iterations),
            "salt": salt,
        }

    def verify(self, password, encoded):
        decoded = self.decode(encoded)
        encoded_2 = self.encode(password, decoded["salt"], decoded["iterations"])
        return encoded == encoded_2

    def safe_summary(self, encoded):
        decoded = self.decode(encoded)
        return {
            "algorithm": decoded["algorithm"],
            "iterations": decoded["iterations"],
            "salt": mask_hash(decoded["salt"]),
            "hash": mask_hash(decoded["hash"]),
        }

    def must_update(self, encoded):
        decoded = self.decode(encoded)
        update_salt = must_update_salt(decoded["salt"], self.salt_entropy)
        return (decoded["iterations"] != self.iterations) or update_salt

    def harden_runtime(self, password, encoded):
        decoded = self.decode(encoded)
        extra_iterations = self.iterations - decoded["iterations"]
        if extra_iterations > 0:
            self.encode(password, decoded["salt"], extra_iterations)


def generate_token(user_id):
    return jwt.encode(
        {"id": user_id, "exp": datetime.now(tz=timezone.utc) + timedelta(days=1)},
        key=SECRET_KEY,
        algorithm="HS256",
    )


def jwt_user(conn: Request, db: DbClient = Depends()) -> Optional[User]:
    if "Authorization" not in conn.headers:
        raise HTTPException(status_code=401, detail="invalid authorization token")

    authorization = conn.headers["Authorization"]
    if (not authorization) or (not authorization.startswith("Bearer ")):
        raise HTTPException(status_code=401, detail="invalid authorization token")
    token = authorization.removeprefix("Bearer ")

    try:
        payload = jwt.decode(token, key=SECRET_KEY, algorithms="HS256")
    except jwt.InvalidTokenError:
        raise HTTPException(status_code=401, detail="invalid authorization token")
    if "id" not in payload:
        raise AuthenticationError(status_code=401, detail="invalid authorization token")

    user = db.get_user(id=payload["id"])
    if user is None:
        raise AuthenticationError(status_code=401, detail="invalid authorization token")

    return user
