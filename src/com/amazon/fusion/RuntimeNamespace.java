// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Namespace.NsBinding;

interface RuntimeNamespace   // TODO FUSION-49 does this extend Store?
{
    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    void bindPredefined(NsBinding binding, Object value);
    // TODO FUSION-48 don't retain Bindings in compiled form

    Object lookup(int address);
}
