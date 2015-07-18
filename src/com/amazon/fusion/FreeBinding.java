// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class FreeBinding
    extends Binding
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
        // This works due to symbol text interning.
        return myName == name;
    }


    @Override
    public boolean sameTarget(Binding other)
    {
        if (this == other) return true;
        if (! (other instanceof FreeBinding)) return false;

        FreeBinding that = (FreeBinding) other;

        // This works due to symbol text interning.
        return this.myName == that.myName;
    }


    @Override
    public Object lookup(Namespace ns)
    {
        return null;
    }

    @Override
    CompiledForm compileDefine(Evaluator eval,
                               Environment env,
                               SyntaxSymbol id,
                               CompiledForm valueForm)
        throws FusionException
    {
        return env.namespace().compileDefine(eval, this, id, valueForm);
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
        return env.namespace().compileFreeTopReference(id);
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
