#!/bin/bash

./boot && ./configure && ./hadrian/build -j && ./hadrian/build test --test-speed=fast && ./hadrian/build stage2:exe:ghc-bin
