// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Set;

final class LocalEnvironment
    implements Environment
{
    static final class LexicalBinding implements Binding
    {
        static final LexicalBinding[] EMPTY_ARRAY = new LexicalBinding[0];

        private final SyntaxSymbol myIdentifier;
        private final int myAddress;

        private LexicalBinding(SyntaxSymbol identifier, int address)
        {
            myIdentifier = identifier;
            myAddress = address;
        }


        @Override
        public Binding originalBinding()
        {
            return this;
        }


        @Override
        public FusionValue lookup(Environment store)
        {
            return store.lookup(this);
        }


        @Override
        public boolean equals(Object other)
        {
            return this == other;
        }

        @Override
        public String toString()
        {
            return "{{LexicalBinding " + myIdentifier + "}}";
        }


        /** Extract the names from an array of bindings. */
        static String[] toNames(LexicalBinding[] bindings)
        {
            if (bindings == null || bindings.length == 0)
            {
                return FusionUtils.EMPTY_STRING_ARRAY;
            }
            else
            {
                String[] names = new String[bindings.length];
                for (int i = 0; i < bindings.length; i++)
                {
                    names[i] = bindings[i].myIdentifier.stringValue();
                }
                return names;
            }
        }
    }


    /** Not null */
    private final Environment      myEnclosure;
    private final LexicalBinding[] myBindings;
    private final FusionValue[]    myValues;


    /** Expand-time environment construction */
    LocalEnvironment(Environment enclosure,
                     SyntaxSymbol[] identifiers)
    {
        myEnclosure = enclosure;

        int count = identifiers.length;
        myBindings = new LexicalBinding[count];
        for (int i = 0; i < count; i++)
        {
            SyntaxSymbol identifier = identifiers[i];

            // This helps make sure we're not preparing the same code twice.
            assert identifier.getBinding() == null
                : "Identifier " + identifier + " already bound to " +
                  identifier.getBinding();

            myBindings[i] = new LexicalBinding(identifier, i);
        }

        myValues = null;
    }

    /** Run-time environment construction */
    LocalEnvironment(Environment enclosure,
                     LexicalBinding[] bindings,
                     FusionValue[] values)
    {
        assert bindings.length == values.length;
        myEnclosure = enclosure;
        myBindings = bindings;
        myValues = values;
    }


    @Override
    public Namespace namespace()
    {
        return myEnclosure.namespace();
    }

    @Override
    public Namespace runtimeNamespace()
    {
        return myEnclosure.namespace();
    }


    @Override
    public Binding substitute(Binding binding, Set<Integer> marks)
    {
        for (LexicalBinding b : myBindings)
        {
            Binding resolvedBoundId = b.myIdentifier.resolve();
            if (resolvedBoundId.equals(binding))
            {
                Set<Integer> boundMarks = b.myIdentifier.computeMarks();
                if (marks.equals(boundMarks))
                {
                    return b;
                }
            }
        }
        return binding;
    }


    void bind(int address, FusionValue value)
    {
        myValues[address] = value;
    }


    @Override
    public FusionValue lookup(Binding binding)
    {
        // Sometimes this is called during prepare pass, when there are not
        // any values bound.
        if (myValues != null && binding instanceof LexicalBinding)
        {
            int address = ((LexicalBinding) binding).myAddress;
            if (address < myBindings.length)
            {
                Binding localBinding = myBindings[address];
                if (binding.equals(localBinding))
                {
                    return myValues[address];
                }
            }
        }
        return myEnclosure.lookup(binding);
    }
}
