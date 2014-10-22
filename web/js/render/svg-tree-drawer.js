/**
 * SVG Tree Drawer <http://github.com/westonruter/svg-tree-drawer/>
 * by Weston Ruter <http://weston.ruter.net/>, 2009
 * License: GPL 3.0 <http://www.gnu.org/licenses/gpl.html>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * @todo Diagrammer: SVG + XML + XSLT + MathML + ContentEditable + hashchange + JSON.parse/stringify
 * @todo Get the text vertical spacing working
 * @todo Add a style property which gets assigned to the <g>
 * @todo Add the ability to draw lines between any two nodes, curved or straight: each node would need an id
 * @todo When collapsing a node that has leaf nodes at different depts, treat it like it has been extended.
 */
(function(){
if(typeof TreeDrawer != 'undefined')
	return;

var svgns = 'http://www.w3.org/2000/svg';
var xlinkns = 'http://www.w3.org/1999/xlink';

/**
 * Class that is associated with a given SVG element and contains methods and
 * properties that relate to the tree as a whole.
 * @todo Should draw() automatically be called?
 */
var T = window.TreeDrawer = function(fallbackElement, treeData){
	if(typeof fallbackElement == 'string')
		fallbackElement = document.getElementById(fallbackElement);
	if(!fallbackElement || !fallbackElement.nodeType == 1)
		throw Error("The param 'fallbackElement' is not valid.");
	this.root = treeData;
	
	var isNativeSVG = !!document.createElementNS(svgns, 'text').getComputedTextLength;
	
	// Create the SVG document
	if(isNativeSVG){
		var svg = document.createElementNS(svgns, 'svg');
		svg.id = fallbackElement.id;
		svg.setAttribute('width', 0);
		svg.setAttribute('height', 0);
		fallbackElement.parentNode.replaceChild(svg, fallbackElement);
		this.svgElement = svg;
		
		// Add the stylesheet
		var defs = document.createElementNS(svgns, 'defs');
		var style = document.createElementNS(svgns, 'style');
		style.setAttribute('type', 'text/css');
		//style.appendChild(document.createTextNode(this.cssStylesheet));
		defs.appendChild(style);
		this.svgElement.appendChild(defs);
		style.appendChild(document.createTextNode(this.cssStylesheet))
		//for(var i = 0, len = this.cssStyleRules.length; i < len; i++){
			//style.sheet.insertRule(this.cssStyleRules[i], i);
			//console.info(this.cssStyleRules[i])
		//}
		
		//if(treeData)
		//	this.populate(treeData);
	}
	// Utilize svgweb
	else {
		//if(typeof svgweb == 'undefined')
		throw Error("The SVGWeb implementation is not currently ready.");
		
		var obj = document.createElement('object', true);
		obj.setAttribute('type', 'image/svg+xml');
		//obj.setAttribute('data', 'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg"></svg>');
		obj.setAttribute('data', 'blank.svg');
		obj.setAttribute('width', 0);
		obj.setAttribute('height', 0);
		this.svgObject = obj;
		var that = this;
		obj.addEventListener('load', function(e){ //TODO: We need to get rid of this async event
			try{
				that.svgDocument = this.contentDocument;
				that.svgElement = that.svgDocument.documentElement;
			
				// Add the stylesheet
				var defs = document.createElementNS(svgns, 'defs');
				var style = document.createElementNS(svgns, 'style');
				style.setAttribute('type', 'text/css');
				style.appendChild(document.createTextNode(that.cssStylesheet));
				
				defs.appendChild(style);
				//that.svgElement.appendChild(defs); //TODO
				
				if(treeData)
					that.populate(treeData);
			}
			catch(e){
				console.error(e)
			}
		}, false);
		svgweb.appendChild(obj, svgContainerElement);
	}
};

//T.prototype.svgDocument = null; //readonly (only set when using svgweb)
//T.prototype.svgObject = null; //readonly (only set when using svgweb)
T.prototype.svgElement = null; //readonly
T.prototype.collapsed = false; //readonly
T.prototype.width = 0; //readonly
T.prototype.height = 0; //readonly
T.prototype.isDrawn = false;

//T.prototype.cssStylesheet = 'line, path { dominant-baseline:middle; }';
T.prototype.cssStylesheet = [
	"line, path, polyline { stroke-width:1px; stroke:black; }",
	"text { dominant-baseline:text-after-edge !important; }", //TODO: Does not work in WebKit
	//"svg {font-size:20px; }",
	//"svg > g > g > text { font-size:120px; }"
].join("\n");

/**
 * The root TreeDrawer.Node
 */
T.prototype.root = null;


/**
 * Blow away all of the nodes
 */
T.prototype.empty = function empty(){
	var svg = this.svgElement;
	//while(svg.firstChild){
	//	svg.parentNode.removeChild(svg.firstChild)
	//}
	for(var i = 0; i < svg.childNodes.length; i++){
		if(svg.childNodes[i].nodeName.toLowerCase() == 'g'){
			svg.removeChild(svg.childNodes[i]);
			i--;
		}
	}
	
	//Now that all of the nodes have been removed from the document, make sure
	// that references to those nodes are removed as well
	function freeMemory(treeNode){
		treeNode.branchElement = null;
		treeNode.labelElement = null;
		forEach(treeNode.children, freeMemory); //recursive
	};
	freeMemory(this.root);
	this.isDrawn = false;
	
	//Give the SVG image zero dimensions
	this.width = 0;
	this.height = 0;
	if(this.svgObject){
		this.svgObject.width = this.width;
		this.svgObject.height = this.height;
	}
	else {
		this.svgElement.setAttribute('width', this.width);
		this.svgElement.setAttribute('height', this.height);
	}
};


/**
 * Renders the tree onto the SVG canvas, resizing the canvas as necessary
 * This function does the heavy lifting of the code
 * @param optional treeData The data structure to be drawn; if not specified, uses this.root
 * @todo Should we save the data before redrawing? In case of error, we could then restore the original?
 * @see _drawNode()
 */
T.prototype.draw = function draw(treeData){
	var isRefresh = (!treeData && this.isDrawn);
	
	//Get the tree data set up
	if(treeData instanceof Object)
		this.root = treeData;
	if(!this.root)
		throw Error("No tree data has been supplied.");
	if(!(this.root instanceof TN))
		this.root = new TN(this.root);
	
	//If we're not doing a refresh, the blow away the existing nodes
	if(!isRefresh && this.isDrawn)
		this.empty();
	
	//var fontSize = parseFloat(window.getComputedStyle(this.svgElement, null).fontSize);
	this.height = this.width = 0; //reset
	
	var info = _drawNode(this, isRefresh, this.svgElement, 0, this.root, 0, 0, this.labelPadding, this.branchHeight);
	
	//Fire 'done' event
	this.isDrawn = true;
	
	if(this.svgObject){
		this.svgObject.width = this.width;
		this.svgObject.height = this.height;
	}
	else {
		this.svgElement.setAttribute('width', this.width);
		this.svgElement.setAttribute('height', this.height);
	}
};


//Die for loops, die!
var forEach = Array.forEach || function forEach(object, block, context) {
	for (var i = 0; i < object.length; i++) {
		block.call(context, object[i], i, object);
	}
};


/**
 * Depending on the type of element, get its width and height
 */
function getDimensions(el){
	try {
		if(el.getBBox){
			return el.getBBox();
		}
		else if(el.getBoundingClientRect){
			var _rect = el.getBoundingClientRect();
			var rect = {
				width: _rect.width || el.offsetWidth || el.clientWidth,
				height: _rect.height || el.offsetHeight || el.clientHeight,
				x: _rect.x,
				y: _rect.y
			};
			//if(!rect.width)
			//	rect.width = el.offsetWidth;
			//if(!rect.height)
			//	rect.height = el.offsetHeight;
			if(isNaN(rect.width) || isNaN(rect.height))
				throw Error("getBoundingClientRect() didn't return the width or height! Are you using an old version of Firefox?");
			return rect;
		}
		//else if(el.width && el.width.baseVal && el.height && el.height.baseVal){
		//	return {
		//		width:el.width.baseVal.value,
		//		height:el.height.baseVal.value,
		//		x:el.x.baseVal.value,
		//		y:el.y.baseVal.value
		//	}
		//}
		//else if(el.offsetParent){
		//	return {
		//		width:el.offsetWidth,
		//		height:el.offsetHeight
		//	}
		//}
	}
	catch(e){
		return {
			width:0,
			height:0,
			x:null,
			y:null
		};
	}
	throw Error("Unable to determine the element's dimensions");
}
TreeDrawer._getElementDimensions = getDimensions;


/**
 * This is necessary because el.getBBox().x is not always the same as el.x.baseVal.value
 */
function getX(el){
	var baseValX = el.x.baseVal;
	if(baseValX.getItem)
		return baseValX.getItem(0).value;
	else
		return baseValX.value;
}


/**
 * This is necessary because el.getBBox().y is not always the same as el.y.baseVal.value
 */
function getY(el){
	var baseValY = el.y.baseVal;
	if(baseValY.getItem)
		return baseValY.getItem(0).value;
	else
		return baseValY.value;
}

/**
 * For when a node is collapsed, we need to gather up all of the leaf nodes
 * @todo What happens when isRefresh?
 */
//function gatherLeafNodes(treeNode){
//	var children = [];
//	if(treeNode.children && treeNode.children.length){
//		forEach(treeNode.children, function(tn){
//			forEach(gatherLeafNodes(tn), function(leaf){
//				children.push(leaf);
//			});
//		});
//	}
//	else {
//		children.push(treeNode);
//	}
//	return children;
//}

/**
 * Recursive function called by TreeDrawer.draw()
 * @param offsetTop The distance from the top to the bottom of the lower end of the branches
 * @todo In Firefox <3 getBoundingClientRect doesn't include width and height
 * @todo <line> should be <path> instead
 */
function _drawNode(tree, isRefresh, parentElement, siblingPosition, treeNode, offsetLeft, offsetTop, isAncestorExtended, collapsedAncestor){
	var g, label, labelRect, labelFontSize, branch, branchHeight, labelPadding;
	
	//if(collapsedAncestor && treeNode.children && treeNode.children.length)
	//	throw Error("Unexpected that tree node has children; all of the leaf nodes have been gathered.");
	
	var isNodeDisplayed = !collapsedAncestor || collapsedAncestor && (!treeNode.children || !treeNode.children.length);
	
	treeNode.leafDecendantsInfo = []; //Reset
	
	//Get or create the node container
	if(isRefresh){
		g = treeNode.labelElement.parentNode;
	}
	else {
		g = document.createElementNS(svgns, 'g');
		if(!treeNode.children || !treeNode.children.length)
			g.setAttribute('class', 'leaf');
		parentElement.appendChild(g);
	}
	var gClass = g.hasAttribute('class') ? g.getAttribute('class').replace(/\s*collapsed/) : '';
	g.setAttribute('class', treeNode.collapsed ? gClass + " collapsed" : gClass);
	
	//Get or create the branch which will connect this label with the parent label
	if(isRefresh){
		branch = treeNode.branchElement;
	}
	else if(parentElement.localName != 'svg'){ //not the root
		branch = document.createElementNS(svgns, 'polyline');
		branch.setAttribute('class', branch.hasAttribute('class') ? branch.getAttribute('class') + " branch" : "branch");
		treeNode.branchElement = branch;
		g.appendChild(branch);
	}
	if(branch)
		branch.style.display = isNodeDisplayed ? '' : 'none';
	if(isNodeDisplayed && branch){
		var branchStyle = window.getComputedStyle(branch, null);
		branchHeight = parseFloat(branchStyle.fontSize) || 0;
		offsetTop += branchHeight;
	}
	
	//Get or make the label
	if(isRefresh){
		label = treeNode.labelElement;
		label.style.display = isNodeDisplayed ? '' : 'none';
		//The dimensions of the foreignObject may have been changed, so re-get
		if(label.nodeName.toLowerCase() == 'foreignobject'){
			labelRect = getDimensions(label.firstChild);
			label.setAttribute('width', labelRect.width);
			label.setAttribute('height', labelRect.height);
		}
	}
	else {
		var sourceLabel = _applyFilters.apply(tree, ['label', treeNode.label, treeNode]);
		if(sourceLabel.nodeType == 1/*Element*/) {
			if(sourceLabel.namespaceURI == svgns){
				//@todo Should this be wrapped in a <g> element so we can translate its position?
				label = sourceLabel;
				g.appendChild(label);
			}
			else {
				label = document.createElementNS(svgns, 'foreignObject');
				// Set width/height to non-zero value so that display isn't disabled;
				// after the label is inserted into the SVG tree, then the offsetHeight
				// and offsetWidth will be used to provide the proper dimensions.
				// This is to facilitate writing CSS style rule selectors.
				label.setAttribute('width', 1);
				label.setAttribute('height', 1);
				label.appendChild(sourceLabel);
				g.appendChild(label);
				
				// Now that element has been inserted into the DOM, calculate the
				// size of the foreignObject's contents, and use these as the width
				// and height.
				if(isNodeDisplayed){
					labelRect = getDimensions(treeNode.label);
					label.setAttribute('width', labelRect.width);
					label.setAttribute('height', labelRect.height);
				}
			}
		}
		else {
			label = document.createElementNS(svgns, 'text');
			label.appendChild(document.createTextNode(sourceLabel.toString(), true));
			g.appendChild(label);
		}
		label.setAttribute('class', label.hasAttribute('class') ? label.getAttribute('class') + " label" : "label");
		treeNode.labelElement = label;
		g.svgTreeDrawerNode = treeNode;
		//TODO: Allow this node to be filtered before insertion (i.e. replace with foreignobject)
	}
	label.style.display = isNodeDisplayed ? '' : 'none';
	if(!labelRect)
		labelRect = getDimensions(label);
	
	//Get styles and dimensions
	var labelStyle = window.getComputedStyle(label, null);
	labelFontSize = parseFloat(labelStyle.fontSize) || 0;
	labelPadding = {
		top:parseFloat(labelStyle.paddingTop) || 0,
		right:parseFloat(labelStyle.paddingRight) || 0,
		bottom:parseFloat(labelStyle.paddingBottom) || 0,
		left:parseFloat(labelStyle.paddingLeft) || 0
	};
	
	//Process each of the children
	var subtreeElements = [label];
	var leafNodes = []; //{label:, branch:}
	if(branch)
		subtreeElements.push(branch);
	var childrenWidth = 0;
	var childrenInfo = [];
	treeNode.maxOffsetTop = offsetTop;
	
	//Get the nodes that appear as children under this node
	var childTreeNodes = treeNode.children;
	
	for(var i = 0, len = childTreeNodes.length; i < len; i++){ //forEach
		var childInfo = _drawNode(
			tree,
			isRefresh,
			g,
			i, //siblingPosition
			childTreeNodes[i],
			offsetLeft + childrenWidth,
			/*collapsedAncestor ? offsetTop :*/ offsetTop + labelPadding.top + labelRect.height + labelPadding.bottom, //+ branchHeight //value of child branch's height is added
			treeNode.extended || isAncestorExtended,
			collapsedAncestor || (treeNode.collapsed ? treeNode : null)  //must be collapsed parent node!
		);
		childrenWidth += childInfo.width;
		
		forEach(childInfo.subtreeElements, function(el){
			subtreeElements.push(el);
		});
		forEach(childInfo.leafNodes, function(el){
			leafNodes.push(el);
		});
		treeNode.maxOffsetTop = Math.max(treeNode.maxOffsetTop, childInfo.maxOffsetTop); //TODO
		childrenInfo.push(childInfo);
	}
	
	//Get coordinates for label and position
	var labelY = offsetTop + labelPadding.top;
	var labelX;
	//If there are children, then x is in the middle of their first and last children
	//TODO: if labelRect.width > childrenWidth, we could pass in the labelRect.width
	var decendantsInfo = treeNode.collapsed && treeNode.leafDecendantsInfo.length ? treeNode.leafDecendantsInfo : childrenInfo;
	if(decendantsInfo.length){
		var leftX, rightX;
		var firstInfo = decendantsInfo[0];
		var lastInfo = decendantsInfo[decendantsInfo.length-1];
		if(firstInfo.label == lastInfo.label){
			leftX = rightX = getX(firstInfo.label) + firstInfo.labelRect.width/2;
		}
		else {
			leftX = getX(firstInfo.label) /*+ firstInfo.labelRect.width*/;
			rightX = getX(lastInfo.label) + lastInfo.labelRect.width;
		}
		labelX = Math.max(
			0,
			leftX + (rightX - leftX)/2 - labelRect.width/2,
			offsetLeft + labelPadding.left
		);
		
		// If the children were narrower than the the parent label, then distribute
		// the children out under the parent. Requires that all subtree graphic
		// elements to be shifted over to the right
		var labelWidthBeyondChildrenWidth = labelRect.width + labelPadding.left + labelPadding.right - childrenWidth;
		if(labelWidthBeyondChildrenWidth > 0){
			var shiftLeft = labelWidthBeyondChildrenWidth/(decendantsInfo.length+1);
			forEach(decendantsInfo, function(child, i){
				forEach(child.subtreeElements, function(el){
					if(el.namespaceURI == svgns){
						var elDimensions = getDimensions(el);
						var thisShiftLeft = shiftLeft*(i+1)/* - elDimensions.width/2*/;
						switch(el.localName.toLowerCase()){
							case 'circle':
								el.cx.baseVal.value += thisShiftLeft;
								break;
							case 'line':
								el.x1.baseVal.value += thisShiftLeft;
								el.x2.baseVal.value += thisShiftLeft;
								break;
							case 'path':
							case 'polygon':
							case 'polyline':
								for(var j = 0, points = el.points, len = points.numberOfItems; j < len; j++){
									points.getItem(j).x += thisShiftLeft;
								}
								break;
							case 'rect':
							default:
								var elRect = getDimensions(el);
								el.setAttribute('x', elRect.x + thisShiftLeft);
								break;
						}
					}
					else {
						throw Error("Expected an SVG element to shift.");
					}
				});
			});
		}
	}
	//Leaf node: no children, so left edge is simply offsetLeft
	else {
		labelX = offsetLeft + labelPadding.left;
		leafNodes.push({
			label:label,
			branch:branch,
			offsetTop:offsetTop
		});
	}
	
	//Add to the x/y position 
	if(label.namespaceURI == svgns){
		switch(label.localName.toLowerCase()){
			case 'text':
				label.setAttribute('x', labelX + 'px');
				label.setAttribute('y', (labelY + labelFontSize) + 'px'); //Cannot be + labelRect.height
				break;
			case 'circle':
				label.setAttribute('cx', (labelX + labelRect.width/2) + 'px');
				label.setAttribute('cy', (labelY + labelRect.height/2) + 'px');
				break;
			//case 'path':
			//	break;
			//case 'polygon':
			//	break;
			case 'rect':
			case 'foreignobject':
				label.setAttribute('x', labelX + 'px');
				label.setAttribute('y', labelY + 'px');
				break;
			default:
				throw Error("Cannot use a '" + label.nodeName + "' element as a node label.");
		}
	}
	else {
		throw Error("Expected an SVG element to position.");
	}
	
	
	//var rect = document.createElementNS(svgns, 'rect');
	//rect.setAttribute('x', labelX + 'px');
	//rect.setAttribute('y', (offsetTop + labelPadding.top) + 'px');
	//rect.setAttribute('height', labelFontSize + 'px');
	//rect.setAttribute('width', labelRect.width + 'px');
	//rect.setAttribute('style', 'fill:none; stroke:red; stroke-width:1px;');
	//g.appendChild(rect);

	//TEMP: offsetLeft
	//var line = document.createElementNS(svgns, 'line');
	//line.setAttribute('style', 'stroke:red; stroke-width:2px; fill:none;');
	//line.setAttribute('x1', offsetLeft + 'px');
	//line.setAttribute('x2', offsetLeft + 'px');
	//line.setAttribute('y1', offsetTop + labelPadding.left + 'px');
	//line.setAttribute('y2', offsetTop + labelPadding.left + labelRect.height + 'px');
	//g.appendChild(line);
	
	
	//Position branch directly above the label
	var branchX = (labelX + labelRect.width/2);
	if(branch){
		branch.points.clear();
		var point;
		if(!collapsedAncestor) {
			point = tree.svgElement.createSVGPoint();
			point.x = branchX;
			point.y = offsetTop;
			branch.points.appendItem(point);
		}
	}
	
	if(isNodeDisplayed){
		//Connect the triangle under collapsed node
		if(treeNode.collapsed){
			var point;
			
			//Set the left bottom point for each branch; this is to help with
			// keeping track of the 
			//forEach(treeNode.leafDecendantsInfo, function(info){
			//	point = tree.svgElement.createSVGPoint();
			//	point.x = getX(firstLeafInfo.label);
			//	info.branch.points.appendItem(point);
			//});
			
			var firstLeafInfo = treeNode.leafDecendantsInfo[0];
			var lastLeafInfo = treeNode.leafDecendantsInfo[treeNode.leafDecendantsInfo.length-1];
			
			var leftBottomX = getX(firstLeafInfo.label);
			var rightBottomX = getX(lastLeafInfo.label)+lastLeafInfo.labelRect.width;
			
			
			//Left bottom
			point = tree.svgElement.createSVGPoint();
			point.x = leftBottomX;
			point.y = firstLeafInfo.offsetTop;
			firstLeafInfo.branch.points.appendItem(point);
			
			//Right bottom
			point = tree.svgElement.createSVGPoint();
			point.x = rightBottomX;
			point.y = lastLeafInfo.offsetTop;
			firstLeafInfo.branch.points.appendItem(point);
			
			//Top
			point = tree.svgElement.createSVGPoint();
			point.x = branchX;
			point.y = offsetTop + labelPadding.top + labelPadding.bottom + labelRect.height;
			firstLeafInfo.branch.points.appendItem(point);
			
			//Closepath
			point = tree.svgElement.createSVGPoint();
			point.x = leftBottomX;
			point.y = firstLeafInfo.offsetTop;
			firstLeafInfo.branch.points.appendItem(point);
		}
		//Connect branches from child labels to parent label
		else if(!collapsedAncestor){
			for(var i = 0, len = childrenInfo.length; i < len; i++){
				var childBranch = childrenInfo[i].branch;
				var point = tree.svgElement.createSVGPoint();
				point.x = branchX;
				point.y = offsetTop + labelPadding.top + labelPadding.bottom + labelRect.height;
				childBranch.points.appendItem(point);
				
				//If odd number, then this should be collapsed!
				if(childBranch.points.numberOfItems % 2 == 1){
					throw Error("Unexpected");
				}
			}
		}
	}
	
		
	// If all of the leaves are supposed to be at the same vertical axis, then
	// push them down now
	if(childrenInfo.length && !isAncestorExtended && treeNode.extended){
		//@todo: Increase the y/y2 of all leafNodes... can we just increment?
		//@todo: Make the svg image higher if it gets higher if one of the leafNodes is taller!!!
		forEach(leafNodes, function(leafNode){
			//Get the points that are on the bottom half
			var bottomPoints = [];
			for(var j = 0, points = leafNode.branch.points, len = points.numberOfItems; j < len; j++){
				bottomPoints.push(points.getItem(j));
			}
			bottomPoints.sort(function(a,b){
				return b.y - a.y;
			});
			bottomPoints.pop();
			
			//Reposition the branch bottom points and the label
			var offsetTopDiff = treeNode.maxOffsetTop - leafNode.offsetTop;
			var newOffsetTop = leafNode.offsetTop + offsetTopDiff;
			if(Math.round(newOffsetTop) <= Math.round(treeNode.maxOffsetTop)){
				//leafNode.branch.y2.baseVal.value = newBranchY;
				forEach(bottomPoints, function(point){
					point.y = newOffsetTop;
				});
				leafNode.label.setAttribute('y', getY(leafNode.label) + offsetTopDiff);
				leafNode.offsetTop += offsetTopDiff;
			}
		});
	}
	
	//Update the dimensions of the entire "canvas"
	tree.height = Math.max(
		tree.height,
		offsetTop + labelPadding.top + labelRect.height + labelPadding.bottom,
		offsetTop + labelPadding.top + labelFontSize + labelPadding.bottom //@todo
	);
	tree.width = Math.max(
		tree.width,
		offsetLeft + childrenWidth,
		offsetLeft + labelRect.width + labelPadding.left + labelPadding.right,
		labelX + labelRect.width + labelPadding.left + labelPadding.right
	);
	
	var info = {
		label:label,
		labelRect:labelRect,
		labelX:labelX, //@todo: this should be part of labelRect
		labelY:labelY, //@todo: this should be part of labelRect
		offsetTop:offsetTop,
		maxOffsetTop:treeNode.maxOffsetTop,
		branch:branch,
		leafNodes:leafNodes,
		subtreeElements:subtreeElements,
		width:Math.max(labelRect.width + labelPadding.left + labelPadding.right, childrenWidth)
	};
	
	if(isNodeDisplayed && collapsedAncestor)
		collapsedAncestor.leafDecendantsInfo.push(info);
	
	return info;
}


/**
 * Class that represents a node in a tree
 */
var TN = T.Node = function(obj){
	this.label = obj.label;
	this.collapsed = !!obj.collapsed;
	this.extended = !!obj.extended;
	
	if(obj.children && obj.children.length){
		this.children = [];
		for(var i = 0, len = obj.children.length; i < len; i++){
			this.children.push(new TN(obj.children[i]));
		}
	}
};
TN.prototype.label = "";
TN.prototype.collapsed = false;
TN.prototype.extended = false;
TN.prototype.children = [];
TN.prototype.maxOffsetTop = 0; //readonly
TN.prototype.branchElement = null;
TN.prototype.labelElement = null;
TN.prototype.leafDecendantsInfo = [];

/**
 * If the node is collapsed, then expand it and draw.
 * @todo We need to keep track of the containing tree because there is no this.draw()
 */
//TN.prototype.expand = function expand(){
//	if(!this.collapsed)
//		return;
//	this.collapsed = false;
//	this.draw();
//};


/**
 * If the node is expanded, then collapse it and draw.
 * @todo We need to keep track of the containing tree because there is no this.draw()
 */
//TN.prototype.collapse = function collapse(){
//	if(this.collapsed)
//		return;
//	tree.draw();
//};


/**
 * Filters and actions (inspired by WordPress)
 */
T.prototype.filters = {};


/**
 * Applied onto the tree as: _applyFilters.apply(this, [hookname, value, ...])
 */
function _applyFilters(hookname, value /*...*/){
	var filters = this.filters[hookname];
	if(filters && filters.length){
		var filterArgs = [];
		for(var i = 1; i < arguments.length; i++)
			filterArgs.push(arguments[i]);
		
		var that = this;
		forEach(filters, function(filter){
			filterArgs[0] = filter.apply(that, filterArgs);
		});
		value = filterArgs[0];
	}
	return value;
}


/**
 * Add a filter callback for a particular hook
 */
T.prototype.addFilter = function(hookname, callback, position){
	if(!this.filters[hookname])
		this.filters[hookname] = [];
	if(isNaN(position))
		this.filters[hookname].push(callback);
	else
		this.filters[hookname].splice(position, 0, callback);
};
T.prototype.actions = {};


/*
1. Convert SVG to Canvas via standard canvas API informed by positions and dimensions 
   available from SVG? Note Canvas has a measureText() method. 
   http://uupaa-js-spinoff.googlecode.com/svn/trunk/uupaa-excanvas.js/demo/8_2_canvas_measureText.html
2. Do SVG and the have a button to export to Canvas, which will iterate over all of the
   elements in the SVG document and draw them onto a corresponding canvas element.


 
 
NOTE: Must be valid JSON, otherwise someone could inject some bad JavaScript in a bad URL
QUESTION: Can we do Packer without the self-extraction code included? We can do a JavaScript implementation of GZip and then store result in hash after Base64


*/

})();

