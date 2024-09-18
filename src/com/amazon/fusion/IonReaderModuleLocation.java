// Copyright (c) 2013-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;

/**
 *
 */
final class IonReaderModuleLocation
    extends ModuleLocation
{
    private final IonReader  mySource;

    IonReaderModuleLocation(IonReader source, SourceName name)
    {
        super(name);
        mySource = source;
    }


    @Override
    IonReader read(Evaluator eval)
    {
        return mySource;
    }
}
