// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import java.util.Set;

final class LocalEnvironment
    implements Environment
{
    static final class LocalBinding implements Binding
    {
        static final LocalBinding[] EMPTY_ARRAY = new LocalBinding[0];

        private final SyntaxSymbol myIdentifier;
        private final int          myDepth;
        private final int          myAddress;

        private LocalBinding(SyntaxSymbol identifier, int depth, int address)
        {
            assert depth > 0;
            myIdentifier = identifier;
            myDepth      = depth;
            myAddress    = address;
        }

        @Override
        public String getName()
        {
            return myIdentifier.stringValue();
        }

        @Override
        public Binding originalBinding()
        {
            return this;
        }


        @Override
        public FusionValue lookup(Environment env)
        {
            return env.lookup(this);
        }


        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            int rib = env.getDepth() - myDepth;
            return new CompiledLocalVariableReference(rib, myAddress);
        }

        @Override
        public CompiledForm compileSet(Evaluator eval, Environment env,
                                       CompiledForm valueForm)
            throws FusionException
        {
            int rib = env.getDepth() - myDepth;
            return new CompiledLocalVariableSet(rib, myAddress, valueForm);
        }


        @Override
        public boolean equals(Object other)
        {
            return this == other;
        }

        @Override
        public String toString()
        {
            return "{{LocalBinding " + myIdentifier + "}}";
        }


        /** Extract the names from an array of bindings. */
        static String[] toNames(LocalBinding[] bindings)
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
    private final Environment    myEnclosure;
    private final Namespace      myNamespace;
    private final int            myDepth;
    private final LocalBinding[] myBindings;


    /** Expand-time environment construction */
    LocalEnvironment(Environment enclosure,
                     SyntaxSymbol[] identifiers)
    {
        myEnclosure = enclosure;
        myNamespace = enclosure.namespace();
        myDepth = 1 + enclosure.getDepth();

        int count = identifiers.length;
        myBindings = new LocalBinding[count];
        for (int i = 0; i < count; i++)
        {
            SyntaxSymbol identifier = identifiers[i];

            // This helps make sure we're not preparing the same code twice.
            assert identifier.getBinding() == null
                : "Identifier " + identifier + " already bound to " +
                  identifier.getBinding();

            myBindings[i] = new LocalBinding(identifier, myDepth, i);
        }
    }


    @Override
    public Namespace namespace()
    {
        return myNamespace;
    }

    @Override
    public int getDepth()
    {
        return myDepth;
    }


    @Override
    public Binding substitute(Binding binding, Set<Integer> marks)
    {
        for (LocalBinding b : myBindings)
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


    @Override
    public FusionValue lookup(Binding binding)
    {
        return myEnclosure.lookup(binding);
    }


    //========================================================================


    // TODO optimize for rib zero which should be a very common case.

    private static final class CompiledLocalVariableReference
        implements CompiledForm
    {
        private final int myRib;
        private final int myAddress;

        CompiledLocalVariableReference(int rib, int address)
        {
            assert rib >= 0;
            myRib     = rib;
            myAddress = address;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object result = store.lookup(myRib, myAddress);
            assert result != null
                : "No value for rib " + myRib + " address " + myAddress;
            return result;
        }
    }


    private static final class CompiledLocalVariableSet
        implements CompiledForm
    {
        private final int          myRib;
        private final int          myAddress;
        private final CompiledForm myValueForm;

        CompiledLocalVariableSet(int rib, int address, CompiledForm valueForm)
        {
            assert rib >= 0;
            myRib       = rib;
            myAddress   = address;
            myValueForm = valueForm;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object value = eval.eval(store, myValueForm);
            store.set(myRib, myAddress, value);
            return UNDEF;
        }
    }
}
