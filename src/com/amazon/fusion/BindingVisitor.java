// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Vistor for traversing a set of environment bindings.
 */
interface BindingVisitor
{
    void visitBinding(String name, FusionValue value);
}
