// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
    public boolean isFree(String name)
    {
        // TODO FUSION-47 intern symbol names and use ==
        return myName.equals(name);
    }


    @Override
    public Binding originalBinding()
    {
        return this;
    }


    @Override
    public boolean sameTarget(Binding other)
    {
        if (this == other) return true;
        if (! (other instanceof FreeBinding)) return false;

        FreeBinding that = (FreeBinding) other;

        // TODO FUSION-47 intern symbol names and use ==
        return this.myName.equals(that.myName);
    }


    @Override
    public Object lookup(Environment store)
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
    public CompiledForm compileTopReference(Evaluator eval, Environment env,
                                            SyntaxSymbol id)
        throws FusionException
    {
        throw new UnboundIdentifierFailure(null, id);
    }

    @Override
    public CompiledForm compileSet(Evaluator eval, Environment env,
                                   CompiledForm valueForm)
        throws FusionException
    {
        String message =
            "Free binding shouldn't make it to compilation: " + this;
        throw new IllegalStateException(message);
    }


    @Override
    public boolean equals(Object other)
    {
        throw new UnsupportedOperationException();
    }


    @Override
    public String toString()
    {
        return "{{FreeBinding " + myName + "}}";
    }
}
