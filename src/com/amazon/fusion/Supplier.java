// Copyright (c) 2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Simulates Java 8's {@code java.util.function.Supplier}.
 * <p>
 * <b>Not for application use!</b>
 */
interface Supplier<T>
{
    T get();
}
