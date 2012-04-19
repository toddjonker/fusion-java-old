// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 *
 */
abstract class NamedValue
    extends FusionValue
{
    private String myName;


    @Override
    final String getInferredName()
    {
        return myName;
    }

    @Override
    final void inferName(String name)
    {
        if (myName == null)
        {
            myName = name;
        }
    }

    final String getEffectiveName()
    {
        String name = getInferredName();
        return (name == null ? "_" : name);
    }
}
