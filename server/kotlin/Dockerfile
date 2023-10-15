ARG VERSION=17

FROM eclipse-temurin:${VERSION}-jdk as build

COPY --link ./gradle /goban/gradle
COPY --link ./gradlew /goban/gradlew
WORKDIR /goban
RUN ./gradlew

# COPY --link ./build /goban/build
# COPY --link .gradle /goban/.gradle
COPY *.gradle.kts /goban

RUN ./gradlew --no-daemon --build-cache --configuration-cache bootJar || echo "Failed build, as expected"

COPY --link ./src /goban/src
# This doesn't actually avoid the daemon :(
RUN ./gradlew --no-daemon --build-cache --configuration-cache bootJar

FROM eclipse-temurin:${VERSION}-jre

COPY --from=build /goban/build/libs/go-recordkeeper-0.0.1-SNAPSHOT.jar /bin/runner/go-recordkeeper.jar
COPY --from=build /goban/build/reports /reports
WORKDIR /bin/runner

CMD ["java", "-jar", "go-recordkeeper.jar"]
