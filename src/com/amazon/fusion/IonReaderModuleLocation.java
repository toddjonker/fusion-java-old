// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;

/**
 *
 */
final class IonReaderModuleLocation
    extends ModuleLocation
{
    private final IonReader  mySource;
    private final SourceName myName;

    IonReaderModuleLocation(IonReader source, SourceName name)
    {
        mySource = source;
        myName = name;
    }

    @Override
    SourceName sourceName()
    {
        return myName;
    }

    @Override
    IonReader read(Evaluator eval)
    {
        return mySource;
    }
}
