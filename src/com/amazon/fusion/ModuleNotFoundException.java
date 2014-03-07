// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Indicates failure to locate a required module.
 */
@SuppressWarnings("serial")
public class ModuleNotFoundException
    extends FusionException
{
    ModuleNotFoundException(String message)
    {
        super(message != null ? message : "Module not found");
    }

    ModuleNotFoundException(String message, SyntaxValue location)
    {
        this(message);
        addContext(location);
    }
}
