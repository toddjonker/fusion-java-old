// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.IOException;

/**
 * Abstract location of module source code.
 *
 * "ModuleResource" might be a better name.
 */
abstract class ModuleLocation
{
    /**
     * @return may be null.
     */
    abstract SourceName sourceName();


    /**
     * This method may only be called once per instance.
     * The result may be positioned on the value to be read.
     */
    abstract IonReader read(Evaluator eval)
        throws IOException;


    String parentDirectory()
    {
        return null;
    }


    @Override
    public String toString()
    {
        SourceName name = sourceName();
        return (name == null ? super.toString() : name.toString());
    }
}
