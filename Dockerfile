FROM ubuntu:20.04
RUN apt-get update -qq && apt-get -y install ca-certificates && rm -rf /var/lib/apt/lists/*
RUN bash -c "echo deb [trusted=yes] https://github.com/hasktorch/libtorch-binary-for-ci/releases/download/apt ./ > /etc/apt/sources.list.d/libtorch.list"

RUN apt-get update -qq && apt-get install -y libtorch=1.8.1+cpu-1 && rm -rf /var/lib/apt/lists/*
RUN apt-get update -qq && apt-get install -y curl && rm -rf /var/lib/apt/lists/*
#COPY ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/serving-0.0.0.0/x/example/build/example/example /usr/local/bin
RUN curl -o /usr/local/bin/example https://github.com/hasktorch/hasktorch-serving-models-skeleton/releases/download/0.1/example 
RUN chmod +x /usr/local/bin/example
EXPOSE 8081 80/tcp
CMD /usr/local/bin/example
