import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
  id("org.springframework.boot") version "3.1.4"
  id("io.spring.dependency-management") version "1.1.3"
  kotlin("jvm") version "1.9.10"
  kotlin("plugin.spring") version "1.9.10"
}

group = "com.chiquito.recordkeeper"

version = "0.0.1-SNAPSHOT"

java { sourceCompatibility = JavaVersion.VERSION_17 }

repositories { mavenCentral() }

dependencies {
  implementation("org.springframework.boot:spring-boot-starter-web:3.1.4")
  implementation("org.springframework.security:spring-security-crypto:6.1.4")
  implementation("com.fasterxml.jackson.module:jackson-module-kotlin:2.15.3")
  implementation("org.jetbrains.kotlin:kotlin-reflect:1.9.10")
  implementation("org.jetbrains.kotlinx:kotlinx-datetime:0.4.1")
  implementation("io.github.oshai:kotlin-logging-jvm:5.1.0")
  implementation("com.auth0:java-jwt:4.4.0")
  implementation("org.postgresql:postgresql:42.6.0")
  testImplementation("org.springframework.boot:spring-boot-starter-test:3.1.4")
  testImplementation("org.junit.jupiter:junit-jupiter-params:5.10.0")
}

tasks.withType<KotlinCompile> {
  kotlinOptions {
    freeCompilerArgs += "-Xjsr305=strict"
    jvmTarget = "17"
  }
}

tasks.withType<Test> { useJUnitPlatform() }
