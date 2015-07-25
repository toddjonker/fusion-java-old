// Copyright (c) 2014-2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Simulates Java 8's {@code java.util.function.Predicate}.
 * <p>
 * <b>Not for application use!</b>
 */
interface Predicate<T>
{
    boolean test(T t);
}
