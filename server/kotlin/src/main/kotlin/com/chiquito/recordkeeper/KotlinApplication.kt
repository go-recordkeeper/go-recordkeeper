package com.chiquito.recordkeeper

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication class KotlinApplication

fun main(args: Array<String>) {
  print("yeeeeeeeeeeeeeeey")
  runApplication<KotlinApplication>(*args) {}
}
