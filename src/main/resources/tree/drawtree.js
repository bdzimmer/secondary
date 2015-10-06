// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Draw family trees with d3.
// Based on code from this stackoverflow question:
// http://stackoverflow.com/questions/31245751/how-do-you-create-a-family-tree-in-d3-js


var boxWidth = 100, boxHeight = 50;
var bendLocation = 0.3;

var connectX = function (d) {
    return d.x - boxWidth / 2;
};

var connectY = function (d) {
    return d.y - boxHeight / 2;
};

var textX = function (d) {
    return d.x - boxWidth / 2 + 5;
};

var textY = function (d) {
    return d.y - 10;
};

var tooltip = d3.select("body").append("div")
  .attr("class", "tooltip")
  .style("position", "absolute")
  .style("opacity", 0);

function drawTree(root, id, width, height)  {

  // zoomable SVG
  var svg = d3.select(id)
    .append("svg")
    .attr("width", width + "px")
    .attr("height", height + "px")
    .call(d3.behavior.zoom().on("zoom", function () {
      svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
    }))
    .append("g")
    
  var allNodes = flatten(root);

  // compute layout
  var tree = d3.layout.tree().size([width - boxWidth, height - boxHeight]),
    nodes = tree.nodes(root),
    links = tree.links(nodes);

  // add link lines
  svg.selectAll(".link")
    .data(links)
    .enter().append("path")
    .attr("class", "link")
    .attr("d", elbow);

  var nodes = svg.selectAll(".node")
    .data(nodes)
    .enter();

  // add spouse lines
  svg.selectAll(".spouse")
    .data(spouses)
    .enter().append("path")
    .attr("class", "spouse")
    .attr("d", spouseLine);

  
  // probably a better way to do this with groups
  
  // add node rectangles
  nodes.append("rect")
    .attr("class", "node")
    .attr("height", boxHeight)
    .attr("width", boxWidth)
    .attr("id", function (d) {
      return d.id;
    })
    .attr("display", function (d) {
      if (d.hidden) {
        return "none"
      } else {
        return ""
      };
    })
    .attr("x", connectX)
    .attr("y", connectY)
    .on("mouseenter", mouseover)
    .on("mouseleave", mouseout)
    .on("mousemove", mousemove);


  // add node labels (names)
  nodes.append("text")
    .attr("class", "nodetext")
    .text(function (d) {
      return d.name;
    })
    .attr("x", textX)
    .attr("y", textY)
    .on("mouseenter", mouseover)
    .on("mouseleave", mouseout)
    .on("mousemove", mousemove);


}

// draw a spouse line
function spouseLine(d) {

  //start point
  var startNode = allNodes.filter(function (v) {
    return (d.srcId == v.id);
  });
  //end point
  var endNode = allNodes.filter(function (v) {
    return (d.dstId == v.id);
  });

  // line start and end
  var lineData = [{x: startNode[0].x, y: startNode[0].y},
                  {x: endNode[0].x, y: endNode[0].y}];

  var fun = d3.svg.line()
    .x(function (d) { return d.x; })
    .y(function (d) { return d.y; })
    .interpolate("linear");

  return fun(lineData);
}


// flatten the node list
// I think I can get rid of the stuff to add node ids.
function flatten(root) {
  var n = [], i = 0;

  function recurse(node) {
    if (node.children) node.children.forEach(recurse);
    if (!node.id) node.id = ++i;
    n.push(node);
  }
  recurse(root);

  return n;
}


// draw right angle lines
function elbow(d, i) {

  if (d.target.no_parent) {
      return "M0,0L0,0";
  }

  var diff = d.source.y - d.target.y;
  var ny = d.target.y + diff * bendLocation;

  lineData = [{x: d.target.x, y: d.target.y},
              {x: d.target.x, y: ny},
              {x: d.source.x, y: d.source.y}]

  var fun = d3.svg.line()
  .x(function (d) { return d.x; })
  .y(function (d) { return d.y; })
  .interpolate("step-after");
  
  return fun(lineData);
}

// show / hide tooltip when mousing
function mouseover(d) {
  tooltip.html("Testing " + d.id)
    .style("opacity", 1);
}

function mouseout(d) {
  tooltip.html("").style("opacity", 0);
}

function mousemove(d) {
  tooltip
    .style("left", d3.event.pageX + "px")
    .style("top",  d3.event.pageY + "px");
}



// Currently not using
/*
d3.selection.prototype.moveToFront = function() {
  return this.each(function(){
    this.parentNode.appendChild(this);
  });
};

d3.selection.prototype.moveToBack = function() { 
    return this.each(function() { 
        var firstChild = this.parentNode.firstChild; 
        if (firstChild) { 
            this.parentNode.insertBefore(this, firstChild); 
        } 
    }); 
};
*/	
