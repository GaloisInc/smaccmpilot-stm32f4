#!/usr/bin/env python

'''
parse a MAVLink protocol XML file and generate a python implementation

Copyright Andrew Tridgell 2011
Copyright Pat Hickey Jan 2013
Released under GNU GPL version 3 or later

'''
import sys, textwrap, os

# allow import from the parent directory to find mavgen 
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))

import mavparse
import smavgen_ivory

sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'lib'))

from genxmlif import GenXmlIfError
from minixsv import pyxsval 

# XSD schema file
schemaFile = os.path.join(os.path.dirname(os.path.realpath(__file__)), "mavschema.xsd")

def mavgen(opts, args) :
    xml = []

    for fname in args:
        print("Validating %s" % fname)
        mavgen_validate(fname, schemaFile, opts.error_limit);

        print("Parsing %s" % fname)
        xml.append(mavparse.MAVXML(fname, opts.wire_protocol))

    # expand includes
    for x in xml[:]:
        for i in x.include:
            fname = os.path.join(os.path.dirname(x.filename), i)

            ## Validate XML file with XSD file
            print("Validating %s" % fname)
            mavgen_validate(fname, schemaFile, opts.error_limit);

            ## Parsing
            print("Parsing %s" % fname)
            xml.append(mavparse.MAVXML(fname, opts.wire_protocol))

            # include message lengths and CRCs too
            for idx in range(0, 256):
                if x.message_lengths[idx] == 0:
                    x.message_lengths[idx] = xml[-1].message_lengths[idx]
                    x.message_crcs[idx] = xml[-1].message_crcs[idx]
                    x.message_names[idx] = xml[-1].message_names[idx]

    # work out max payload size across all includes
    largest_payload = 0
    for x in xml:
        if x.largest_payload > largest_payload:
            largest_payload = x.largest_payload
    for x in xml:
        x.largest_payload = largest_payload

    if mavparse.check_duplicates(xml):
        sys.exit(1)

    print("Found %u MAVLink message types in %u XML files" % (
        mavparse.total_msgs(xml), len(xml)))

    smavgen_ivory.generate_messages(opts.output, xml)

def mavgen_validate(fname, schema, errorLimitNumber) :
    """Uses minixsv to validate an XML file with a given XSD schema file."""
    # use default values of minixsv, location of the schema file must be specified in the XML file
    domTreeWrapper = pyxsval.parseAndValidate(fname, xsdFile=schema, errorLimit=errorLimitNumber)
            
    # domTree is a minidom document object
    domTree = domTreeWrapper.getTree()


if __name__=="__main__":
    from optparse import OptionParser

    parser = OptionParser("%prog [options] <XML files>")

    parser.add_option("-o", "--output", dest="output", default="Mavlink",
                    help="output directory for SMACCMPilot.Mavlink module")
    parser.add_option("--error-limit", dest="error_limit", default=200,
                    help="maximum number of validation errors")

    (opts, args) = parser.parse_args()

    if len(args) < 1:
        args = ["../../message_definitions/common.xml"]
    opts.wire_protocol = '1.0'
    mavgen(opts, args)

