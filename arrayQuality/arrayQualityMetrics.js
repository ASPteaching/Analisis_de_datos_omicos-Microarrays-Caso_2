// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, true, false, false, true, true, true, true, true, true, true, true, true, true, true ];
var arrayMetadata    = [ [ "1", "S1.SINT", "GSM5374839_S1.CEL", "SINT", "red" ], [ "2", "S2.SINT", "GSM5374840_S2.CEL", "SINT", "red" ], [ "3", "S3.SINT", "GSM5374841_S3.CEL", "SINT", "red" ], [ "4", "S4.SINT", "GSM5374842_S4.CEL", "SINT", "red" ], [ "5", "S5.SINT", "GSM5374843_S5.CEL", "SINT", "red" ], [ "6", "S6.SINT", "GSM5374844_S6.CEL", "SINT", "red" ], [ "7", "S7.SINT", "GSM5374845_S7.CEL", "SINT", "red" ], [ "8", "S8.SINT", "GSM5374846_S8.CEL", "SINT", "red" ], [ "9", "S9.SINT", "GSM5374847_S9.CEL", "SINT", "red" ], [ "10", "S10.SINT", "GSM5374848_S10.CEL", "SINT", "red" ], [ "11", "S11.SINT", "GSM5374849_S11.CEL", "SINT", "red" ], [ "12", "S12.ASINT", "GSM5374850_S12.CEL", "ASINT", "green" ], [ "13", "S13.ASINT", "GSM5374851_S13.CEL", "ASINT", "green" ], [ "14", "S14.ASINT", "GSM5374852_S14.CEL", "ASINT", "green" ], [ "15", "S15.ASINT", "GSM5374853_S15.CEL", "ASINT", "green" ], [ "16", "S16.ASINT", "GSM5374854_S16.CEL", "ASINT", "green" ], [ "17", "S17.ASINT", "GSM5374855_S17.CEL", "ASINT", "green" ], [ "18", "S18.ASINT", "GSM5374856_S18.CEL", "ASINT", "green" ], [ "19", "S19.ASINT", "GSM5374857_S19.CEL", "ASINT", "green" ], [ "20", "S20.ASINT", "GSM5374858_S20.CEL", "ASINT", "green" ], [ "21", "S21.ASINT", "GSM5374859_S21.CEL", "ASINT", "green" ], [ "22", "S22.ASINT", "GSM5374860_S22.CEL", "ASINT", "green" ], [ "23", "S23.ASINT", "GSM5374861_S23.CEL", "ASINT", "green" ], [ "24", "S24.ASINT", "GSM5374862_S24.CEL", "ASINT", "green" ], [ "25", "S25.ASINT", "GSM5374863_S25.CEL", "ASINT", "green" ], [ "26", "S26.ASINT", "GSM5374864_S26.CEL", "ASINT", "green" ], [ "27", "S27.ASINT", "GSM5374865_S27.CEL", "ASINT", "green" ], [ "28", "S28.ASINT", "GSM5374866_S28.CEL", "ASINT", "green" ], [ "29", "S29.ASINT", "GSM5374867_S29.CEL", "ASINT", "green" ], [ "30", "S30.SANO", "GSM5374868_S30.CEL", "SANO", "blue" ], [ "31", "S31.SANO", "GSM5374869_S31.CEL", "SANO", "blue" ], [ "32", "S32.SANO", "GSM5374870_S32.CEL", "SANO", "blue" ], [ "33", "S33.SANO", "GSM5374871_S33.CEL", "SANO", "blue" ], [ "34", "S34.SANO", "GSM5374872_S34.CEL", "SANO", "blue" ], [ "35", "S35.SANO", "GSM5374873_S35.CEL", "SANO", "blue" ], [ "36", "S36.SANO", "GSM5374874_S36.CEL", "SANO", "blue" ], [ "37", "S37.SANO", "GSM5374875_S37.CEL", "SANO", "blue" ], [ "38", "S38.SANO", "GSM5374876_S38.CEL", "SANO", "blue" ], [ "39", "S39.SANO", "GSM5374877_S39.CEL", "SANO", "blue" ], [ "40", "S40.SANO", "GSM5374878_S40.CEL", "SANO", "blue" ], [ "41", "S41.SANO", "GSM5374879_S41.CEL", "SANO", "blue" ], [ "42", "S42.SANO", "GSM5374880_S42.CEL", "SANO", "blue" ], [ "43", "S43.SANO", "GSM5374881_S43.CEL", "SANO", "blue" ], [ "44", "S44.SANO", "GSM5374882_S44.CEL", "SANO", "blue" ], [ "45", "S45.SANO", "GSM5374883_S45.CEL", "SANO", "blue" ], [ "46", "S46.SANO", "GSM5374884_S46.CEL", "SANO", "blue" ], [ "47", "S47.SANO", "GSM5374885_S47.CEL", "SANO", "blue" ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
    for(i=0; i<ssrules.length; i++) {
        if (ssrules[i].selectorText == (".aqm" + reportObjId)) {
		ssrules[i].style.cssText = cssText[0+status];
		break;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
