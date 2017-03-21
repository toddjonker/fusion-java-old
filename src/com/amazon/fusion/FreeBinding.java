// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionSymbol.BaseSymbol;


final class FreeBinding
    extends Binding
{
    private final BaseSymbol myName;

    FreeBinding(BaseSymbol name)
    {
        myName = name;
    }

    @Override
    public BaseSymbol getName()
    {
        return myName;
    }


    @Override
    public boolean isFree(BaseSymbol name)
    {
        // This works due to symbol interning.
        return myName == name;
    }


    @Override
    public boolean sameTarget(Binding other)
    {
        if (this == other) return true;
        return other.isFree(myName);
    }


    @Override
    public Object lookup(Namespace ns)
    {
        return null;
    }


    @Override
    public String mutationSyntaxErrorMessage()
    {
         return "unbound variable";
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
    Object visit(Visitor v) throws FusionException
    {
        return v.visit(this);
    }

    @Override
    public boolean equals(Object other)
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode()
    {
        return myName.hashCode();
    }

    @Override
    public String toString()
    {
        return "{{FreeBinding " + myName + "}}";
    }
}
