000010 process sql, nodynam, noseq, test
000040 identification division.
000041 program-id.    XADB2TST.
000042 author.        Xact Consulting A/S.
000043******************************************************************
000044* XaTester Sample                                                *
000045* Xact Consulting A/S                                            *
000046* Arnold Nielsens Boulevard 68A                                  *
000047* DK-2650 Hvidovre, Denmark                                      *
000048******************************************************************
000070 environment    division.
000080 configuration  section.
000090 special-names.
000110 input-output   section.
000120 file-control.
000170 data division.
000180 file section.
000267 working-storage section.
000270
000271 01  Dsntiar-areal.
000272     03 dsntiar               pic x(08)      value 'DSNTIAR'.
000273     03 dsntiar-sqlcode       pic ++++9      value zero.
000274     03 db2-logical-msgl      pic s9(8) comp value +78.
000275
000276 01  db2-message-areal.
000277     03 db2-message-length    pic s9(4) comp value +780.
000278     03 db2-tiar-msg-table.
000279        05 db2-tiar-msgs      pic x(78) occurs 10.
000280
000281 01  db2-log-data.
000290     03 db2-abend             pic ++++9  value zero.
000291     03 db2-funk              pic x(08)  value space.
000292     03 db2-kald-nr           pic x(02)  value space.
000293     03 db2-tablename         pic x(32)  value space.
000295     03 returncc              pic s9(4)  value zero comp.
000296     03 wsSqlCode             pic s9(9)  value zero comp-5.
000297     03 wsUserid              pic x(08)  value 'XATESTER'.
000298     03 debug-data            pic x(256) value space.
000381
000382 01  ws-callarea.
000389     03 ws-callarg.
000390        05 ws-module          pic x(08) value space.
000391        05 filler             pic x(01) value space.
000392        05 ws-tstmp           pic x(26) value space.
000393
000394     exec sql include sqlca  end-exec.
000400     exec sql include xaempl end-exec.
003932
003938 linkage section.
005001
005002     copy xadb2tsc.
005003
005018 procedure division using xadb2tst-import xadb2tst-export.
005019 xadb2tst-main Section.
005020
005021     perform Initiering.
005022     perform Behandling.
005023     perform Afslutning.
005024     goback.
005025
005026******************************************************************
005027*    initiering
005028******************************************************************
005029 Initiering section.
005030
005031     Display 'XADB2TST Started with function: '
005032              xadb2tst-imp-function.
005035
005036     initialize xadb2tst-export.
005037
005038     perform varying tally from +1 by +1 until tally > +7
005039        initialize xadb2tst-exp-table (tally)
005040        end-perform.
005050
006297******************************************************************
006298*    Behandling
006299******************************************************************
006300 Behandling section.
006309
006310     move zero  to xadb2tst-exp-count.
006311     move +7    to xadb2tst-exp-maxcount.
006312     move space to xadb2tst-exp-message.
006313
006314     evaluate xadb2tst-imp-function
006315        when 'INSERT' perform DoInsert
006316        when 'DELETE' perform DoDelete
006317        when 'MODIFY' perform DoModify
006318        when 'REVOKE' perform DoRevoke
006319        when 'LIST'   perform DoList
006320        when 'GET'    perform DoGet
006321        when other    move +4 to returncc
006322                      String 'Unknown function:'
006323                              xadb2tst-imp-function
006324                              delimited by size
006325                              into xadb2tst-exp-message
006326        end-evaluate.
006327
006328     if sqlcode not = zero
006329        Perform GetSqlMessage
006330        move debug-data to xadb2tst-exp-message.
006331
006332     move sqlcode    to xadb2tst-exp-errorcode
006333                        dsntiar-sqlcode.
006334
006335     Display 'XADB2TST ' xadb2tst-imp-function
006340             ' ended with sqlcode: '  dsntiar-sqlcode.
007220
007230******************************************************************
007240*    Afslutning
007250******************************************************************
007260 Afslutning section.
007270
007380     display 'XADB2TST Ended with return-code: ' returncc.
007400
007410     move returncc to return-code.
007420
007490******************************************************************
007500*    DoList
007510******************************************************************
007520 DoList Section.
007530
007531     move 'Open'       to db2-funk.
007532     move 'XaEmployee' to db2-tablename.
007533     move 01           to db2-kald-nr.
007534
007535     Exec sql declare c1 cursor for
007536          select rident
007537                ,firstname
007538                ,lastname
007539                ,cprnr
007540                ,tstmp
007541                ,userid
007542                ,statuscode
007544            from xaemployee
007545            where statuscode <> 'D'
007546            order by rident
007547            end-exec.
007548
007549     Exec sql open c1 end-exec.
007550
007551     if sqlcode not = zero
007552        perform dsntiar-log
007553        move +12 to returncc
007554        Go DoList-Exit.
007555
007556*    if xadb2tst-imp-skip > zero
007557*       perform varying tally from +1 by +1
007558*               until tally > xadb2tst-imp-skip
007559*               or sqlcode not = zero
007560*          perform FetchC1
007561*          end-Perform
007562
007563     perform FetchC1.
007564
007565     if sqlcode = zero
007567        perform varying tally from +1 by +1 until tally > +7
007568                                       or sqlcode not = zero
007570           move rident     in dclxaemployee
007571                           to xadb2tst-exp-id (tally)
007572           move firstname  in dclxaemployee
007573                           to xadb2tst-exp-firstname (tally)
007574           move lastname   in dclxaemployee
007575                           to xadb2tst-exp-lastname (tally)
007576           move cprnr      in dclxaemployee
007577                           to xadb2tst-exp-cprnr    (tally)
007578           move tstmp      in dclxaemployee
007579                           to xadb2tst-exp-tstmp    (tally)
007580           move userid     in dclxaemployee
007581                           to xadb2tst-exp-userid   (tally)
007582           move statuscode in dclxaemployee
007583                           to xadb2tst-exp-statuscode(tally)
007584           move tally      to xadb2tst-exp-count
007585           perform FetchC1
007586           end-perform.
007587
007588     Move sqlcode   to wsSqlCode.
007589     Exec sql close c1 end-exec.
007590     Move WsSqlCode to SqlCode.
007591
007592 DoList-Exit.
007593     Exit.
007594******************************************************************
007595*    DoInsert
007596******************************************************************
007597 DoInsert Section.
007598
007599     move 'Insert'     to db2-funk.
007600     move 'XaEmployee' to db2-tablename.
007601     move 01           to db2-kald-nr.
007602
007603     move xadb2tst-imp-rident    to rident    in dclxaemployee
007604     move xadb2tst-imp-firstname to firstname in dclxaemployee.
007605     move xadb2tst-imp-lastname  to lastname  in dclxaemployee.
007606     move xadb2tst-imp-cprnr     to cprnr     in dclxaemployee.
007607
007608     exec sql insert into XaEmployee values (
007609              :dclxaemployee.rident
007610             ,:dclxaemployee.firstname
007611             ,:dclxaemployee.lastname
007612             ,:dclxaemployee.cprnr
007613             ,current timestamp
007614             ,:wsUserid
007615             ,'O')
007616              end-exec.
007617
007618     move sqlcode to xadb2tst-exp-errorcode.
007619
007620     if sqlcode not = zero
007621        perform dsntiar-log
007630        move +12 to returncc.
007700
007800******************************************************************
007900*    DoDelete
008000******************************************************************
008100 DoDelete Section.
008200
008300     move 'Delete'     to db2-funk.
008400     move 'XaEmployee' to db2-tablename.
008410     move 01           to db2-kald-nr.
008420
008430     move xadb2tst-imp-rident    to rident    in dclxaemployee
008443
008444     exec sql update XaEmployee
008445              set userid     = :wsUserid
008446                 ,tstmp      = current timestamp
008448                 ,statuscode = 'D'
008449              where rident   = :dclxaEmployee.rident
008450                and statuscode <> 'D'
008451              end-exec.
008452
008453     move sqlcode to xadb2tst-exp-errorcode.
008454
008455     if sqlcode not = zero
008456        perform dsntiar-log
008457        move +12 to returncc.
008458
008459******************************************************************
008460*    DoModify
008461******************************************************************
008462 DoModify Section.
008463
008464     move 'Update'     to db2-funk.
008465     move 'XaEmployee' to db2-tablename.
008466     move 01           to db2-kald-nr.
008467
008469     move xadb2tst-imp-rident    to rident    in dclxaemployee
008470     move xadb2tst-imp-firstname to firstname in dclxaemployee.
008471     move xadb2tst-imp-lastname  to lastname  in dclxaemployee.
008472
008473     exec sql update XaEmployee
008474              set firstname  = :dclxaemployee.firstname
008475                 ,lastname   = :dclxaemployee.lastname
008476                 ,tstmp      = current timestamp
008477                 ,userid     = :wsUserid
008478                 ,statuscode = 'M'
008479              where rident   = :dclxaemployee.rident
008480                and statuscode <> 'D'
008481              end-exec.
008482
008483     move sqlcode to xadb2tst-exp-errorcode.
008484
008485     if sqlcode not = zero
008486        perform dsntiar-log
008487        move +12 to returncc.
008488
008489******************************************************************
008490*    Dorevoke
008491******************************************************************
008492 DoRevoke Section.
008493
008494     move 'Update'     to db2-funk.
008495     move 'XaEmployee' to db2-tablename.
008496     move 02           to db2-kald-nr.
008497
008498     move xadb2tst-imp-rident to rident in dclxaemployee
008499
008500     exec sql update XaEmployee
008502              set statuscode = 'R'
008503                 ,tstmp = current timestamp
008504              where rident   = :dclxaemployee.rident
008505                and statuscode = 'D'
008506              end-exec.
008507
008508     move sqlcode to xadb2tst-exp-errorcode.
008509
008510     if sqlcode not = zero
008511        perform dsntiar-log
008512        move +12 to returncc.
008513
008514******************************************************************
008515*    fetch a row
008516******************************************************************
008517 DoGet Section.
008518
008519     move 'Select'     to db2-funk.
008520     move 'XaEmployee' to db2-tablename.
008521     move 01           to db2-kald-nr.
008522
008523     move xadb2tst-imp-rident to rident in dclxaemployee
008524
008530     Exec sql select firstname
008700                    ,lastname
008801                    ,cprnr
008810                    ,tstmp
008900                    ,userid
009100                    ,statuscode
009101          into
009110                    :dclxaemployee.firstname
009120                   ,:dclxaemployee.lastname
009130                   ,:dclxaemployee.cprnr
009140                   ,:dclxaemployee.tstmp
009150                   ,:dclxaemployee.userid
009160                   ,:dclxaemployee.statuscode
009161          from xaemployee
009170          where rident = :dclxaemployee.rident
009200          end-exec.
009300
009310     move sqlcode to xadb2tst-exp-errorcode.
009320
009400     if sqlcode not = zero
009500        perform dsntiar-log
009600        move +12 to returncc
009610     else
009611        move +1         to tally
009620        move rident     in dclxaemployee
009630                        to xadb2tst-exp-id (tally)
009640        move firstname  in dclxaemployee
009650                        to xadb2tst-exp-firstname (tally)
009660        move lastname   in dclxaemployee
009670                        to xadb2tst-exp-lastname (tally)
009680        move cprnr      in dclxaemployee
009690                        to xadb2tst-exp-cprnr    (tally)
009691        move tstmp      in dclxaemployee
009692                        to xadb2tst-exp-tstmp    (tally)
009693        move userid     in dclxaemployee
009694                        to xadb2tst-exp-userid   (tally)
009695        move statuscode in dclxaemployee
009696                        to xadb2tst-exp-statuscode(tally)
009697        move tally      to xadb2tst-exp-count.
009700
009800******************************************************************
009900*    fetch a row
010000******************************************************************
010100 FetchC1 Section.
010200
010300     Exec sql fetch c1 into
010400          :dclxaemployee.rident
010500         ,:dclxaemployee.firstname
010600         ,:dclxaemployee.lastname
010700         ,:dclxaemployee.cprnr
010800         ,:dclxaemployee.tstmp
010900         ,:dclxaemployee.userid
011000         ,:dclxaemployee.statuscode
011100         end-exec.
011200
011300     move sqlcode to xadb2tst-exp-errorcode.
011400
011500     if not (sqlcode = zero or +100)
011600        perform dsntiar-log
011700        move +12 to returncc.
011800
049800******************************************************************
049900*    Do the sql-testing
050000******************************************************************
050100 sql-test section.
050200
050300     Perform GetSqlMessage.
050900     perform skriv-log.
051000
051010 GetSqlMessage Section.
051020
051030     move sqlcode to db2-abend.
051031     move space   to debug-data.
051032
051040     string 'Code:'  db2-abend
051042           ' Nr:'    db2-kald-nr
051050           ' Funk:'  db2-funk
051060           ' Table:' db2-tablename
051070             delimited by size into debug-data.
051080
051100******************************************************************
051200*    Log sqlca error messages
051300******************************************************************
051400 dsntiar-log section.
051500
051600     move +78   to db2-logical-msgl.
051700     move +780  to db2-message-length.
051800     move space to db2-tiar-msg-table.
051900
052000     Display 'Statement:' db2-kald-nr
052100             ' Function:' db2-funk
052200             ' Table:'    db2-tablename.
052300
052400     Call 'dsntiar' using sqlca
052500                        db2-message-areal
052600                        db2-logical-msgl
052700          on exception
052800             move sqlcode to dsntiar-sqlcode
052900             display 'XADB2TST Error: Unable to call DSNTIAR'
053000             display 'Sqlcode: ' dsntiar-sqlcode
053100             display 'Sqlca  :'
053200             display sqlca
053300             end-call
053400
053500     perform varying tally from +1 by +1 until tally > +10
053600        if db2-tiar-msgs (tally) not = space
053700           Display db2-tiar-msgs (tally)
053800           end-if
053900        end-perform
054000     .
054100
054200 Skriv-log Section.
054300
054400     Display debug-data.
054500
054600     move space to debug-data.
054700