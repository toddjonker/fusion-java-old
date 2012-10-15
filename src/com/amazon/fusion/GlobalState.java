// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSystem;

/**
 * A bunch of objects from {@link KernelModule} that are needed by other parts
 * of the implementation.
 */
class GlobalState
{
    final IonSystem        myIonSystem;
    final ModuleInstance   myKernelModule;
    final LoadHandler      myLoadHandler;
    final UseKeyword       myUseKeyword;
    final DynamicParameter myCurrentNamespaceParam;


    GlobalState(IonSystem        ionSystem,
                ModuleInstance   kernel,
                LoadHandler      loadHandler,
                UseKeyword       useKeyword,
                DynamicParameter currentNamespaceParam)
    {
        myIonSystem             = ionSystem;
        myKernelModule          = kernel;
        myLoadHandler           = loadHandler;
        myUseKeyword            = useKeyword;
        myCurrentNamespaceParam = currentNamespaceParam;
    }
}
