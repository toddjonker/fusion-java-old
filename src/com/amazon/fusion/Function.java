// Copyright (c) 2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Simulates Java 8's {@code java.util.function.Function}.
 * <p>
 * <b>Not for application use!</b>
 */
interface Function<T, R>
{
    R apply(T t);
}
