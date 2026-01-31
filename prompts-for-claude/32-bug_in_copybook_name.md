If you run the command "python3 kyco-input-data/run_data_division_tree.py" you can test the api "get_data_division_tree" against the cobol source file "EQTRHORI.cbl".

The result contains a node represented by the following json snippet

            {
              "name": "FILLER$179",
              "level": 2,
              "occurs": 50,
              "is_group": true,
              "is_filler": true,
              "line_number": 221,
              "copybook": "COPYBOOK",
              "position": {
                "start": 9734,
                "end": 9842,
                "size": 109
              },
              ...
            }

This snippet says that the "copybook" is "COPYBOOK" while it should have been "ESPCA018".

Other json objects that come before this one and the describe other fillers have the correct "copybook".
For instance, the node preceeding this one is correctly defined by this json snippet:

            {
              "name": "FILLER$177",
              "level": 2,
              "is_group": true,
              "is_filler": true,
              "line_number": 215,
              "copybook": "ESPCA013",
              "position": {
                "start": 1395,
                "end": 1583,
                "size": 189
              },
              ...
            }

Explain the reason of this bug and fix it, without breaking anything.
