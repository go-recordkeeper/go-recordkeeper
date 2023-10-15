package com.chiquito.recordkeeper

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RestController

@RestController
class AuthController {
  @GetMapping("/foo")
  fun foo(): String {
    print("chiquito")
    return "bar"
  }
}
