// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class FreeBinding implements Binding
{
    private final String myName;

    FreeBinding(String name)
    {
        myName = name;
    }

    @Override
    public String getName()
    {
        return myName;
    }


    @Override
    public Binding originalBinding()
    {
        return this;
    }


    @Override
    public FusionValue lookup(Environment store)
    {
        return null;
    }

    @Override
    public CompiledForm compileReference(Evaluator eval, Environment env)
        throws FusionException
    {
        String message =
            "Free binding shouldn't make it to compilation: " + this;
        throw new IllegalStateException(message);
    }


    @Override
    public boolean equals(Object other)
    {
        if (this == other) return true;
        if (! (other instanceof FreeBinding)) return false;

        FreeBinding that = (FreeBinding) other;

        // TODO FUSION-47 intern symbol names and use ==
        return this.myName.equals(that.myName);
    }


    @Override
    public String toString()
    {
        return "{{FreeBinding " + myName + "}}";
    }
}
