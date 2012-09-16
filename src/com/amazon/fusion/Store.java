// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The "memory" holding execution-time data.
 */
interface Store
{
    /* *
     * Assigns a variable a new value.
     * This is the implementation of {@code set!}.
     */
//  void set(int address, Object value);

    /**
     * Finds the value bound at a given address.
     *
     * @param rib the number of lexical binding layers to go "up"
     * @param address the index within the rib
     *
     * @return not null.
     */
    Object lookup(int rib, int address);

    NamespaceStore namespace();
}
