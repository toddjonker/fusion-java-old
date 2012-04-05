// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;

/**
 *
 */
interface Writeable
{
    void write(IonWriter out);
}
