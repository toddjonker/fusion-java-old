// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;


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


    abstract void identify(Appendable out)
        throws IOException;


    @Override
    public final void write(Appendable out)
        throws IOException
    {
        out.append("/* ");
        identify(out);
        out.append(" */");
    }
}
