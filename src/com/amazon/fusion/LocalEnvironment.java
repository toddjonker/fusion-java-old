// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;
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
        public boolean isFree(String name)
        {
            return false;
        }

        @Override
        public Binding originalBinding()
        {
            return this;
        }

        @Override
        public boolean sameTarget(Binding other)
        {
            // Don't need to call other.originalBinding() since locals are
            // never renamed or wrapped.
            return this == other;
        }


        @Override
        public Object lookup(Environment env)
        {
            return env.lookup(this);
        }


        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            int rib = env.getDepth() - myDepth;
            if (rib == 0)
            {
                return new CompiledImmediateVariableReference(myAddress);
            }
            return new CompiledLocalVariableReference(rib, myAddress);
        }

        @Override
        public CompiledForm compileTopReference(Evaluator eval,
                                                Environment env,
                                                SyntaxSymbol id)
            throws FusionException
        {
            String message =
                "#%top not implemented for local binding: " + this;
            throw new SyntaxException("#%top", message, id);
        }

        @Override
        public CompiledForm compileSet(Evaluator eval, Environment env,
                                       CompiledForm valueForm)
            throws FusionException
        {
            int rib = env.getDepth() - myDepth;
            if (rib == 0)
            {
                return new CompiledImmediateVariableSet(myAddress, valueForm);
            }
            return new CompiledLocalVariableSet(rib, myAddress, valueForm);
        }


        @Override
        public boolean equals(Object other)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public String toString()
        {
            return "{{{LocalBinding " + myIdentifier + "}}}";
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
            if (b.myIdentifier.resolvesBound(binding, marks))
            {
                return b;
            }
        }
        return binding;
    }

    @Override
    public Binding substituteFree(String name, Set<Integer> marks)
    {
        for (LocalBinding b : myBindings)
        {
            if (b.myIdentifier.resolvesFree(name, marks))
            {
                return b;
            }
        }
        return null;
    }


    @Override
    public Object lookup(Binding binding)
    {
        return myEnclosure.lookup(binding);
    }


    //========================================================================


    /** Reference to a var in the immediately-enclosing environment. */
    private static final class CompiledImmediateVariableReference
        implements CompiledForm
    {
        private final int myAddress;

        CompiledImmediateVariableReference(int address)
        {
            myAddress = address;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object result = store.lookup(myAddress);
            assert result != null
                : "No value for immediate var @" + myAddress;
            return result;
        }
    }


    private static final class CompiledLocalVariableReference
        implements CompiledForm
    {
        private final int myRib;
        private final int myAddress;

        CompiledLocalVariableReference(int rib, int address)
        {
            assert rib > 0;
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


    private static final class CompiledImmediateVariableSet
        implements CompiledForm
    {
        private final int          myAddress;
        private final CompiledForm myValueForm;

        CompiledImmediateVariableSet(int address, CompiledForm valueForm)
        {
            myAddress   = address;
            myValueForm = valueForm;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object value = eval.eval(store, myValueForm);
            store.set(myAddress, value);
            return voidValue(eval);
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
            assert rib > 0;
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
            return voidValue(eval);
        }
    }
}
