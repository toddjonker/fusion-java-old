// Copyright (c) 2012-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingSite.makeLocalBindingSite;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import java.util.Set;

final class LocalEnvironment
    implements Environment
{
    /**
     * Denotes a binding at some local scope, for example {@code lambda} and
     * {@code letrec} expressions.
     */
    static final class LocalBinding
        extends Binding
    {
        static final LocalBinding[] EMPTY_ARRAY = new LocalBinding[0];

        private final SyntaxSymbol       myIdentifier;
                final int                myDepth;
                final int                myAddress;
        private final BindingSite        mySite;

        private LocalBinding(SyntaxSymbol identifier, int depth, int address)
        {
            assert depth > 0;
            myIdentifier = identifier;
            myDepth      = depth;
            myAddress    = address;
            mySite       = makeLocalBindingSite(identifier.getLocation());
        }

        @Override
        BaseSymbol getName()
        {
            return myIdentifier.getName();
        }

        @Override
        BindingSite getBindingSite()
        {
            return mySite;
        }

        @Override
        boolean sameTarget(Binding other)
        {
            // Don't need to call other.target() since local bindings are
            // original and unique.
            return this == other;
        }


        @Override
        Object lookup(Namespace ns)
        {
            return null;  // We're local, not a namespace binding.
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
            return System.identityHashCode(this);
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


    /**
     * Expand-time environment construction.
     *
     * @throws FusionException if there's a duplicate identifier.
     */
    LocalEnvironment(Environment enclosure,
                     SyntaxSymbol[] identifiers,
                     SyntaxValue formForErrors)
        throws FusionException
    {
        myEnclosure = enclosure;
        myNamespace = enclosure.namespace();
        myDepth = 1 + enclosure.getDepth();

        // 2019-04 Stats from the standard library and test suite:
        //   85% of local envs have 1 entry, 12% have 2.
        //   99% have 3 or fewer entries and none have more than 5.
        int count = identifiers.length;
        if (count > 1)
        {
            // TODO Avoid a hashmap when count==2, do a simple comparison.
            BoundIdMap<SyntaxSymbol> ids = new BoundIdMap<>();
            for (int i = 0; i < count; i++)
            {
                SyntaxSymbol id   = identifiers[i];
                SyntaxSymbol dupe = ids.put(id, id);
                if (dupe != null)
                {
                    String message =
                        "duplicate binding identifier: " +
                        printQuotedSymbol(id.stringValue());

                    SyntaxException ex =
                        new SyntaxException(null, message, id);
                    ex.addContext(formForErrors);
                    throw ex;
                }
            }
        }

        myBindings = new LocalBinding[count];
        for (int i = 0; i < count; i++)
        {
            SyntaxSymbol identifier = identifiers[i];
            myBindings[i] = new LocalBinding(identifier, myDepth, i);
        }
    }


    /**
     * Compile-time environment construction; makes a dummy environment to
     * keep track of the current depth.
     */
    LocalEnvironment(Environment enclosure)
    {
        myEnclosure = enclosure;
        myNamespace = enclosure.namespace();
        myDepth = 1 + enclosure.getDepth();

        myBindings = null;
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
    public Binding substitute(Binding binding, Set<MarkWrap> marks)
    {
        BoundIdentifier boundId = new BoundIdentifier(binding, marks);

        for (LocalBinding b : myBindings)
        {
            if (b.myIdentifier.resolveBoundIdentifier().equals(boundId))
            {
                return b;
            }
        }
        return binding;
    }

    @Override
    public Binding substituteFree(BaseSymbol name, Set<MarkWrap> marks)
    {
        BoundIdentifier boundId =
            new BoundIdentifier(new FreeBinding(name), marks);

        for (LocalBinding b : myBindings)
        {
            if (b.myIdentifier.resolveBoundIdentifier().equals(boundId))
            {
                return b;
            }
        }
        return null;
    }


    @Override
    public String toString()
    {
        StringBuilder buf = new StringBuilder("{{{LocalEnv (");
        boolean first = true;
        for (LocalBinding b : myBindings)
        {
            if (! first) buf.append(' ');
            buf.append(b.myIdentifier);
            first = false;
        }
        buf.append(")}}}");
        return buf.toString();
    }


    //========================================================================


    /** Reference to a var in the immediately-enclosing environment. */
    static final class CompiledImmediateVariableReference
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


    static final class CompiledLocalVariableReference
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


    static final class CompiledImmediateVariableSet
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


    static final class CompiledLocalVariableSet
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
