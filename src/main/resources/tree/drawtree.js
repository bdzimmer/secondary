// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Draw family trees with d3.
// Based on code from this stackoverflow question:
// http://stackoverflow.com/questions/31245751/how-do-you-create-a-family-tree-in-d3-js

// 2015-10-03: Created.
// 2015-10-06: Style fixes. Better mouseovers and links to character pages.
// 2015-10-07: Further updates for preview text. Needs initial zoom / scale based on layout.
// 2015-10-08: Finds initial zoom and scale.
// 2015-10-10: Fixed bugs with initial zoom and scale finding.
// 2016-01-18: Fixed bugs with spouse lines. Line breaks in names. 

var boxWidth = 120, boxHeight = 80;
var bendLocation = 0.4;

var connectX = function (d) {
    return d.x - boxWidth / 2;
};

var connectY = function (d) {
    return d.y - boxHeight / 2;
};

var textX = function (d) {
    return d.x - boxWidth / 2 + 8;
};

var textY = function (d) {
    return d.y - boxHeight / 2 + 0;
};

var preview = d3.select("body").append("div")
  .attr("class", "preview")
  .style("position", "absolute")
  .style("opacity", 0);


function drawTree(root, spouses, id, width, height)  {

  // define layout; compute node and link positions
  var layout = d3.layout.tree().nodeSize([boxWidth * 1.1, boxHeight * 2]);
  var nodes = layout.nodes(root);
  var links = layout.links(nodes);

  // figure out initial translation and scale
  // it appears that the translation and scale have to be applied to both
  // the main svg group and the zoom listener for consistent results.

  var minX = d3.min(nodes, function(d) {return d.x;}) - boxWidth;
  var maxX = d3.max(nodes, function(d) {return d.x;}) + boxWidth;
  var minY = d3.min(nodes, function(d) {return d.y;}) - boxHeight;
  var maxY = d3.max(nodes, function(d) {return d.y;}) + boxHeight;

  var startScale = Math.min(width / (maxX - minX), height / (maxY - minY), 1)
  var transX = 0 - minX * startScale
  var transY = 0 - minY * startScale

  console.log(minX + " " + maxX + " "  + minY + " " + maxY)
  console.log(transX + " " + transY + " " + startScale);

  // create zoom object for initial layout that allows scaling and translation
  var zoom = d3.behavior.zoom()
    .on("zoom", function () {
      svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
  	})
  	.translate([transX, transY])
  	.scale(startScale);

  // create SVG using zoom object
  var svg = d3.select(id)
    .append("svg")
    .attr("width", width + "px")
    .attr("height", height + "px")
    .call(zoom)
    .append("g")
    .attr("transform", "translate(" + [transX, transY] + ") scale(" + startScale + ")");

  // add link lines
  svg.selectAll(".link")
    .data(links)
    .enter().append("path")
    .attr("class", function(d) {
      return d.target.parentType;
    })
    .attr("d", elbow);

  var flatNodes = flatten(root);

  // add spouse lines (double lines)
  var spouseLines = svg.selectAll(".spouse")
    .data(spouses)
    .enter().append("g");

  spouseLines.append("path")
    .attr("class", "spouse")
    .attr("d", function(d) {
      return spouseLine(d, -3, flatNodes);
    });

  spouseLines.append("path")
    .attr("class", "spouse")
    .attr("d", function(d) {
      return spouseLine(d, 0, flatNodes);
    });

  // add node rectangles
  var groups = svg.selectAll(".node")
    .data(nodes)
    .enter().append("a")
    .attr("xlink:href", function(d){return d.id + ".html";});

  groups.append("rect")
    .attr("class", "node")
    .attr("height", boxHeight)
    .attr("width", boxWidth)
    .attr("id", function (d){return d.id;})
    .attr("display", function (d) {
      if (d.nodeType === "character") {
        return "";
      } else {
        return "none";
      };
    })
    .attr("x", connectX)
    .attr("y", connectY)
  	.on("mouseenter", mousenter)
    .on("mouseleave", mouseleave)
    .on("mousemove", mousemove);

  var texts = groups.append("g")
    .attr("transform", function(d) {
      return "translate(" + textX(d) + "," + textY(d) + ")";
    })
    .on("mouseenter", mousenter)
    .on("mouseleave", mouseleave)
    .on("mousemove", mousemove)

  texts.append("text")
    .attr("class", "nodetext")
    .attr("x", 0)
    .selectAll("tspan")
    .data(function (d) {
      return d.name.split("\t");
    }).enter().append("tspan")
      .text(function (d) {return d;})
      .attr("x", 0)
      .attr("dy", "2em");
}


// draw a spouse line
function spouseLine(d, yoff, flatNodes) {

  // start point
  var startNode = flatNodes.filter(function (v) {
    return (d.srcId === v.id);
  });

  // end point
  var endNode = flatNodes.filter(function (v) {
    return (d.dstId === v.id);
  });

  // line start and end
  var lineData = [{x: startNode[0].x, y: startNode[0].y + yoff},
                  {x: endNode[0].x, y: endNode[0].y + yoff}];

  var fun = d3.svg.line()
    .x(function (d) { return d.x; })
    .y(function (d) { return d.y; })
    .interpolate("linear");

  return fun(lineData);
}


// flatten the node list
// I think I can get rid of the stuff to add node ids.
function flatten(root) {
  var n = [];

  function recurse(node) {
    if (node.children) {
    	node.children.forEach(recurse);
    }
    n.push(node);
  }
  recurse(root);

  return n;
}


// draw right angle lines
function elbow(d) {

  if (d.target.parentType === "none") {
      return "M0,0L0,0";
  }

  var diff = d.source.y - d.target.y;
  var ny = d.target.y + diff * bendLocation;

  var lineData = [{x: d.target.x, y: d.target.y},
                  {x: d.target.x, y: ny},
                  {x: d.source.x, y: d.source.y}]

  var fun = d3.svg.line()
  .x(function (d) { return d.x; })
  .y(function (d) { return d.y; })
  .interpolate("step-after");

  return fun(lineData);
}


// mouse event functions

function mousenter(d) {
  preview.html("<h4>" + d.name + "</h4>" + d.description)
    .style("opacity", 1);
}

function mouseleave(d) {
  preview.html("").style("opacity", 0);
}

function mousemove(d) {
  preview
    .style("left", d3.event.pageX + 10 + "px")
    .style("top",  d3.event.pageY + 10 + "px");
}
