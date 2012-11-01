// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class FeatureDocumentation
{
    enum Kind { PROCEDURE, SYNTAX, MODULE }

    final String myName;
    final Kind   myKind;
    final String myUsage;
    final String myBody;

    FeatureDocumentation(String name, Kind kind, String usage, String body)
    {
        myName = name;
        myKind = kind;
        myUsage = usage;
        myBody = body;
    }
}
