// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Syntax wrap that adds all bindings from a specific namespace.
 */
class EnvironmentRenameWrap
    extends SyntaxWrap
{
    private final Environment myEnvironment;

    EnvironmentRenameWrap(Environment environment)
    {
        myEnvironment = environment;
    }


    @Override
    Environment.Binding resolve(String ident)
    {
        return myEnvironment.resolve(ident);
    }

    @Override
    public String toString()
    {
        return "/* Namespace renames */";
    }
}
