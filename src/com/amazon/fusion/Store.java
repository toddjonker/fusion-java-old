// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The "memory" holding execution-time data.
 */
interface Store
{
    /**
     * Finds the value bound to a given name.
     *
     * @param name must not be null or empty.
     *
     * @return the bound value, or null if there's no value.
     */
    FusionValue lookup(Binding binding);


    RuntimeNamespace runtimeNamespace();
}
