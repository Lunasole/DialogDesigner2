EnableExplicit
;{ Common }

	; Few extensions to Trim/LTrim/RTrim functions
	; String		a string to trim
	; Pattern		string to exclude
	; RETURN:		modified string
	Procedure$ RTrimS(string$, pattern$)
		Protected L = Len(pattern$)
		Protected D = @string$ + StringByteLength(string$) - L * SizeOf(Character)
		While CompareMemoryString(D, @pattern$, #PB_String_NoCase, L) = #PB_String_Equal
			PokeC(D, 0)
			D - L * SizeOf(Character)
		Wend
		ProcedureReturn PeekS(@string$)
	EndProcedure

	; equivalent of VB6 Split()
	; sOut$:		array to receive output
	; String$:		string to split
	; Delimiter$:	a sequence of chars to split String$ by
	; Limit:		a maximum number of results, if set, then last item of array contains all data over Limit
	; RETURN:		number of Delimiter$ inside of String$ [also it is size of sOut$ array]
	Procedure SplitS(Array Out$(1), String$, Delimiter$, Limit = -1, Mode = #PB_String_CaseSensitive)
		Protected nC, mPos, lPos = 1, nDelimLen = Len(Delimiter$)
		Repeat
			mPos = FindString(String$, Delimiter$, lPos, Mode)
			If ArraySize(Out$()) < nC
				ReDim Out$(nC + 10240) ; enlarge your array for just $2800 :3
			EndIf
			If mPos And (Limit = -1 Or nC < Limit)
				Out$(nC) = Mid(String$, lPos, (mPos - lPos))
				lPos = mPos + nDelimLen
				nC + 1
			Else
				Out$(nC) = Mid(String$, lPos)
				Break
			EndIf
		ForEver
		ReDim Out$(nC) ; trim output array
		ProcedureReturn nC
	EndProcedure

	; equivalent of VB6 Join()
	; sOut$:		array to receive output
	; MaxCnt		amount of array items used (by default whole array is joined)
	; RETURN:		string containing all array items
	Procedure$ JoinS(Array In$(1), Delimiter$ = "", MaxCnt = -1)
		Protected sOut$, nC, nCMax
		If MaxCnt = -1
			nCMax = ArraySize(In$())
		Else
			nCMax = MaxCnt
		EndIf
		For nC = 0 To nCMax
			If Not nC = nCMax
				sOut$ + In$(nC) + Delimiter$
			Else
				sOut$ + In$(nC)
			EndIf
		Next
		ProcedureReturn sOut$
	EndProcedure

	; Check if string is numeric
	; RETURN:	true if string contains numbers only (or numbers starting with "-"), false else
	Procedure isNumeric(Dat$)
		Protected t, l = Len(dat$)
		Protected c$
		Protected res
		For T = 1 To L
			c$ = Mid(Dat$, T, 1)
			If c$ = "-"
				If T > 1
					res = #False
					Break
				Else
					res = #True
				EndIf
			Else
				Select c$
					Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
						res = #True
					Default
						res = #False
						Break
				EndSelect
			EndIf
		Next T
		ProcedureReturn res
	EndProcedure

	; Check if string is valid PB proc name
	; RETURN:	true if string contains numbers only (or numbers starting with "-"), false else
	Procedure isProc(Dat$)
		Protected Res = #True
		Protected *C.Character = @Dat$
		Select *C\c
			Case 'a' To 'z', 'A' To 'Z', '_':
				While *C\c <> 0
					Select *C\c
						Case 'a' To 'z', 'A' To 'Z', '_', '0' To '9':
							*C + SizeOf(Character)
							Continue
						Default
							res = #False
							Break
					EndSelect
				Wend
			Default
				res = #False
		EndSelect
		ProcedureReturn res
	EndProcedure

	; a simple but nice string XOr function
	; RETURN:		XOred string, call again to XOr it back
	Procedure$ NStr(pNStr$)
		Protected sLen = Len(pNStr$)
		Protected sEn.w, tXr.c = (30 & sLen) + 1
		For sEn = 1 To sLen
			PokeC(@pNStr$ + (sEn - 1)  * SizeOf(tXr), Asc(Mid(pNStr$, sEn, 1)) ! tXr)
		Next
		ProcedureReturn pNStr$
	EndProcedure

;}

;{ DPI scaling }

	Global.f DPIXScale = 0.0, DPIYScale = 0.0
	; Gets scale factor for X and Y for current windows settings
	; RETURN:		none
	Procedure InitScaleDPI()
		Protected dc = GetDC_(#Null)
		Protected lpx, lpy
		If dc
			lpx = GetDeviceCaps_(dc, #LOGPIXELSX)
			lpy = GetDeviceCaps_(dc, #LOGPIXELSY)
			If lpx > 0
				DPIXScale = lpx / 96.0
			EndIf
			If lpy > 0
				DPIYScale = lpy / 96.0
			EndIf
			ReleaseDC_(#Null, dc)
		EndIf
	EndProcedure

	InitScaleDPI()
	Macro ScaleX (x)
		((x) * DPIXScale)
	EndMacro
	Macro ScaleY (y)
		((y) * DPIYScale)
	EndMacro

;}

;{ Menu subsystem ex }

	; Adds new item to current menu
	; RETURN:	none
	Procedure MenuItemEx (ItemID, Text$, Enabled = 1, Visible = 1, Checked = 0, ImageID = 0)
		Global MENU_CURRENT, MENU_CURRENT_HANDLER ; here to decrease arguments count by 2 ^^
		If ItemID = -1
			CallDebugger
		EndIf
		If IsMenu(MENU_CURRENT)
			UnbindMenuEvent(MENU_CURRENT, ItemID, MENU_CURRENT_HANDLER)
			If Visible
				MenuItem(ItemID, Text$, ImageID)
				DisableMenuItem(MENU_CURRENT, ItemID, Bool(Not Enabled))
				SetMenuItemState(MENU_CURRENT, ItemID, Bool(Checked))
				If Enabled
					BindMenuEvent(MENU_CURRENT, ItemID, MENU_CURRENT_HANDLER)
				EndIf
			EndIf
		EndIf
	EndProcedure

	; Add new menu bar if conditions match
	; Active	boolean value, if false then bar is not added
	; RETURN:	none
	Procedure MenuBarEx (Active)
		If Active
			MenuBar()
		EndIf
	EndProcedure

	; Creates popup menu selecting it as current and assigning callback procedure on success
	; RETURN: handle to created menu on success
	Procedure CreatePopupMenuEx (MenuID, *Handler)
		Protected *hMnu = CreatePopupMenu(MenuID)
		If *hMnu ; switch current menu
			MENU_CURRENT = MenuID :		MENU_CURRENT_HANDLER = *Handler
		Else
			MENU_CURRENT = 0 :		MENU_CURRENT_HANDLER = 0
		EndIf
		ProcedureReturn *hMnu
	EndProcedure

	; Used to create nested menus
	; RETURN:	submenu handle on success, 0 on fail or when submenu is not visible
	Procedure OpenSubMenuEx (Text$, Enabled = 1, Visible = 1, Checked = 0, ImageID = 0)
		If Visible And IsMenu(MENU_CURRENT)
			Protected *hSub = OpenSubMenu(Text$, ImageID)
			If *hSub
				DisableMenuItem(MENU_CURRENT, *hSub, Bool(Not Enabled))
				SetMenuItemState(MENU_CURRENT, *hSub, Bool(Checked))
				ProcedureReturn *hSub
			EndIf
		EndIf
	EndProcedure

	; Set text of given menu item
	Procedure SetMenuItemTextEx (ItemID, Text$)
		SetMenuItemText(MENU_CURRENT, ItemID, Text$)
	EndProcedure

;}

;{ Object Definition }

	Structure XML_COMMON_ATTRIBUTES
		width$		; - positive integer value or 0 (default="0") (set the "minimum size" of a control)
		height$
		id$			; - #Number identifier for a gadget or a window (default is #PB_Any if not specified). It can be a runtime constant.
		name$		; - a string identifying the object (for DialogGadget() mainly, case insensitive) (default="")
		text$		; - text string for the object (default="")
		font$		; - text font (CUSTOM ATTRIBUTE)
		forecolor$	; - text color (CUSTOM ATTRIBUTE)
		backcolor$	; - back color (CUSTOM ATTRIBUTE)
		tooltip$	; - tooltip text string (CUSTOM ATTRIBUTE)
		variable$	; - create variable for control in code? yes/no (CUSTOM ATTRIBUTE)
		flags$		; - gadget/window flags in the form "#PB_Window_Borderless | #PB_Window_ScreenCentered"  (default="")
		min$		; - minimum value
		max$		; - maximum value
		value$		; - current value
		invisible$	; - if set to "yes", creates the object invisible (default="no")
		disabled$	; - if "yes", creates the object disabled (gadgets only) (default="no")
		colspan$	; - inside the <gridbox> element only, allows an element to span multiple rows/columns
		rowspan$	; (default="1")
	EndStructure

	Structure XML_EXTRA_WINDOW
		minwidth$	; = 'auto' or a numeric value
		maxwidth$	; = 'auto' or a numeric value
		minheight$	; = 'auto' or a numeric value
		maxheight$	; = 'auto' or a numeric value
	EndStructure
	Structure XML_EXTRA_HVBOX
		spacing$	; = space to add between the packed childs (default=5)
		expand$		; = yes		   - items get bigger to fill all space (default)
		align$		; = top/left	  - only applied when expand="no", top/left is the default
	EndStructure
	Structure XML_EXTRA_GRIDBOX
		columns$	; = number of columns (default = 2)
		colspacing$	; = space to add between columns/rows (default = 5)
		rowspacing$
		colexpand$	; = yes		   - items get bigger to fill all space (default)
		rowexpand$	;	no			- do not expand to fill all space
	EndStructure
	Structure XML_EXTRA_MULTIBOX
	EndStructure
	Structure XML_EXTRA_SINGLEBOX
		margin$ 		; = margin around the content (default = 10)
		expand$ 		; = yes		- expand child to fill all space (default)
		expandwidth$  	; = max size to expand the children to. If the requested size is larger than
		expandheight$	; this setting then the request size is used (ie the content does not get smaller)
		align$  		; = combination of top,left,bottom,right and center. (only effective when expand <> yes)
	EndStructure
	Structure XML_EXTRA_OPTION
		group$			; number to connect options together
	EndStructure
	Structure XML_EXTRA_SCROLLAREA
		scrolling$			; vertical,horizontal or both (default)
		innerheight$		; value (int) or auto(default)
		innerwidth$
		scrollstep$			; original name = "step", value (int)
	EndStructure
	Structure XML_EXTRA_SCROLLBAR
		page$				; page length (int)
	EndStructure
	Structure XML_EXTRA_SPLITTER
		firstmin$			; value or auto (default)
		secondmin$			; value or auto (default)
	EndStructure

	Structure XML_EXTRA_ATTRIBUTES
		type$		; object identifier, one of ObjTypes
		window		.XML_EXTRA_WINDOW
		hvbox		.XML_EXTRA_HVBOX
		gridbox		.XML_EXTRA_GRIDBOX
		multibox	.XML_EXTRA_MULTIBOX
		singlebox	.XML_EXTRA_SINGLEBOX
		option		.XML_EXTRA_OPTION
		scrollarea	.XML_EXTRA_SCROLLAREA
		scrollbar	.XML_EXTRA_SCROLLBAR
		splitter	.XML_EXTRA_SPLITTER
	EndStructure

	Structure XML_EVENTS
		onevent$	  			; = EventProcedure() - generic event binding, for all event type
	EndStructure

	#C_HBOX 			= "hbox"
	#C_VBOX 			= "vbox"
	#C_GRIDBOX 			= "gridbox"
	#C_MULTIBOX			= "multibox"
	#C_SINGLEBOX 		= "singlebox"
	#C_WINDOW 			= "window"
	#C_BUTTON 			= "button"
	#C_BUTTONIMAGE		= "buttonimage"
	#C_CALENDAR 		= "calendar"
	#C_CANVAS 			= "canvas"
	#C_CHECKBOX 		= "checkbox"
	#C_COMBOBOX 		= "combobox"
	#C_CONTAINER		= "container"
	#C_DATE 			= "date"
	#C_EDITOR 			= "editor"
	#C_EXPLORERCOMBO 	= "explorercombo"
	#C_EXPLORERLIST 	= "explorerlist"
	#C_EXPLORERTREE 	= "explorertree"
	#C_FRAME 			= "frame"
	#C_HYPERLINK 		= "hyperlink"
	#C_IPADDRESS 		= "ipaddress"
	#C_IMAGE 			= "image"
	#C_LISTICON 		= "listicon"
	#C_LISTVIEW 		= "listview"
	#C_OPTION 			= "option"
	#C_PANEL 			= "panel"
	#C_PROGRESSBAR 		= "progressbar"
	#C_SCROLLAREA 		= "scrollarea"
	#C_SCROLLBAR 		= "scrollbar"
	#C_SPIN 			= "spin"
	#C_SPLITTER 		= "splitter"
	#C_STRING 			= "string"
	#C_TEXT 			= "text"
	#C_TRACKBAR			= "trackbar"
	#C_TREE 			= "tree"
	#C_WEB 				= "web"
	#C_SCINTILLA 		= "scintilla"
	#C_TAB 				= "tab"
	#C_EMPTY 			= "empty"
	#C_DIALOGS 			= "dialogs"
	#v_name 		= "name"
	#v_id 			= "id"
	#v_disabled 	= "disabled"
	#v_invisible 	= "invisible"
	#v_height 		= "height"
	#v_width 		= "width"
	#v_max 			= "max"
	#v_min 			= "min"
	#v_rowspan 		= "rowspan"
	#v_colspan 		= "colspan"
	#v_flags 		= "flags"
	#v_text 		= "text"
	#v_value 		= "value"
 	#v_onevent 		= "onevent"
 	#v_onevent_wrap	= "dd2events"		; this is reference to #v_onevent used ONLY on export/import, while #v_onevent is used internally in Designer
 	#v_tooltip		= "tooltip"			; a tooltip attribute, missing in dialogs lib. used by DD2, present in exported XML, but not in code XML, affects code generation
 	#v_tooltip_wrap	= "dd2tooltip"
 	#v_variable		= "variable"		; a variable attribute, used by DD2, present in exported XML, but not in code XML, affects code generation
 	#v_variable_wrap= "dd2variable"
 	#v_font			= "font"
 	#v_font_wrap	= "dd2font"
 	#v_forecolor	= "frontcolor"
 	#v_forecolor_wrap	= "dd2frontcolor"
 	#v_backcolor	= "backcolor"
 	#v_backcolor_wrap	= "dd2backcolor"
	#v_minwidth 	= "minwidth"
	#v_maxwidth 	= "maxwidth"
	#v_minheight	= "minheight"
	#v_maxheight	= "maxheight"
	#v_align		= "align"
	#v_expand		= "expand"
	#v_colexpand 	= "colexpand"
	#v_rowexpand 	= "rowexpand"
	#v_spacing		= "spacing"
	#v_colspacing 	= "colspacing"
	#v_rowspacing 	= "rowspacing"
	#v_columns 		= "columns"
	#v_expandheight = "expandheight"
	#v_expandwidth 	= "expandwidth"
	#v_margin 		= "margin"
	#v_group 		= "group"
	#v_scrolling 	= "scrolling"
	#v_innerheight 	= "innerheight"
	#v_innerwidth 	= "innerwidth"
	#v_step 		= "step"
	#v_page 		= "page"
	#v_firstmin 	= "firstmin"
	#v_secondmin 	= "secondmin"
	#v_class		= "class"
	#v_parent 		= "parent"
	#v_childs 		= "childs"
	Structure OBJECT
		treeName$	; object name used as key in tree. it is synchronized with pCOMMON\Name$, but must always be lowercase (because tree is based on map())
		pCOMMON		.XML_COMMON_ATTRIBUTES
		pEXTRA		.XML_EXTRA_ATTRIBUTES
		pEVENTS		.XML_EVENTS
	EndStructure

;}

;{ Tree 1.4 }

	Structure TreeNode
		Data.OBJECT		; pointer for user data. If you want to store other data type here, you only need to adjust code in TreeAdd() function.
		Parent$			; internal, don't touch it
		Name$			; internal, don't edit too
		List Childs$()	; internal also :)
	EndStructure

	; Add new element to a tree data structure. Element is added to the end of current list
	; *Data				pointer to a some user data
	; Location			add item to a beginning or to the end of parent childs list? Use #PB_List_First / #PB_List_Last
	; RETURN:			true on success, false else
	Procedure TreeAdd (Map Tree.TreeNode(), Name$, *Data, Parent$ = "", Location = #PB_List_Last)
		If Name$ = "" Or FindMapElement(Tree(), Name$)
			; name$ is empty or already exists
			ProcedureReturn #False
		EndIf
		If Parent$ And FindMapElement(Tree(), Parent$) = 0
			; non-existing parent specified
			ProcedureReturn #False
		EndIf
		If *Data
			CopyStructure(*Data, @Tree(Name$)\Data, OBJECT)
		EndIf
		Tree(Name$)\Name$ = Name$
		If Parent$
			Tree(Name$)\Parent$ = Parent$
			AddElement(Tree(Parent$)\Childs$())
			Tree(Parent$)\Childs$() = Name$
			If Not Location = #PB_List_First And Not Location = #PB_List_Last
				Location = #PB_List_Last
			EndIf
			MoveElement(Tree(Parent$)\Childs$(), Location)
		EndIf
		ProcedureReturn #True
	EndProcedure

	; Deletes given element and (optionally) all nested elements
	; Name$				identifier
	; DeleteAllChilds	true to delete all nested elements with their subelements
	; RETURN:			true on success, false else
	Procedure TreeDelete (Map Tree.TreeNode(), Name$, DeleteAllChilds = #False)
		If Name$ = "" Or FindMapElement(Tree(), Name$) = 0
			; element not exists
			ProcedureReturn #False
		EndIf
		Protected Parent$ = Tree(Name$)\Parent$
		Protected tKey$
		ForEach Tree(Name$)\Childs$()
			tKey$ = Tree(Name$)\Childs$()
			If DeleteAllChilds
				If TreeDelete(Tree(), tKey$, DeleteAllChilds)
					ResetList(Tree(Name$)\Childs$())
				EndIf
			Else
				Tree(tKey$)\Parent$ = Parent$
				If Parent$
					AddElement(Tree(Parent$)\Childs$())
					Tree(Parent$)\Childs$() = tKey$
					MoveElement(Tree(Parent$)\Childs$(), #PB_List_Last)
				EndIf
			EndIf
		Next
		If Parent$
			ForEach Tree(Parent$)\Childs$()
				If Tree(Parent$)\Childs$() = Name$
					DeleteElement(Tree(Parent$)\Childs$())
					Break
				EndIf
			Next
		EndIf
		DeleteMapElement(Tree(), Name$)
		ProcedureReturn #True
	EndProcedure

	; Moves given element to a new position
	; Name$				item name to move, cannot be empty
	; Target$			where to move? cannot be empty, except if TargetAsParent = #true (then "" can be used to move item to the root level)
	; TargetAsParent	set it to True if element should be added as a child of Target element, instead of add it at the same tree level
	; Location			one of four #PB_List_ constants. By default element will be moved to a position after Target
	;					NOTE: if TargetAsParent = true, then location can be only #PB_List_First or #PB_List_Last
	; RETURN:			true on success
	Procedure TreeMove (Map Tree.TreeNode(), Name$, Target$, TargetAsParent = #False, Location = #PB_List_After)
		If Name$ = ""  Or (Target$ = "" And Not TargetAsParent)
			; Name$ can't be empty, Target$ can't be empty unless TargetAsParent mode enabled
			ProcedureReturn #False
		ElseIf FindMapElement(Tree(), Name$) = 0 Or (Target$ <> "" And FindMapElement(Tree(), Target$) = 0)
			; One or both items not found
			ProcedureReturn #False
		ElseIf Name$ = Target$
			; That futile search for meaning
			ProcedureReturn #True
		EndIf
		Protected CName$ = Target$
		Repeat
			If CName$
				If CName$ = Name$
					; yes, it is
					ProcedureReturn #False
				EndIf
				CName$ = Tree(CName$)\Parent$
			Else
				Break
			EndIf
		ForEver
		Protected Target
		Protected *CParent.TreeNode
		Protected NameParent$ = Tree(Name$)\Parent$
		Protected TargetParent$
		If TargetAsParent
			TargetParent$ = Target$
			If Not Location = #PB_List_First And Not Location = #PB_List_Last
				Location = #PB_List_Last
			EndIf
		Else
			TargetParent$ = Tree(Target$)\Parent$
			If Not Location = #PB_List_First And Not Location = #PB_List_Last And Not Location = #PB_List_Before And Not Location = #PB_List_After
				Location = #PB_List_After
			EndIf
		EndIf
		If NameParent$
			*CParent = @Tree(NameParent$)
			ForEach *CParent\Childs$()
				If *CParent\Childs$() = Name$
					DeleteElement(*CParent\Childs$())
					Break
				EndIf
			Next
		EndIf
		If TargetParent$
			*CParent = @Tree(TargetParent$)
			If Not TargetAsParent
				ForEach *CParent\Childs$()
					If *CParent\Childs$() = Target$
						Target = @*CParent\Childs$()
						AddElement(*CParent\Childs$())
						*CParent\Childs$() = Name$
						MoveElement(*CParent\Childs$(), Location, Target)
						Break
					EndIf
				Next
			Else
				AddElement(*CParent\Childs$())
				*CParent\Childs$() = Name$
				MoveElement(*CParent\Childs$(), Location)
			EndIf
		EndIf
		Tree(Name$)\Parent$ = TargetParent$
		ProcedureReturn #True
	EndProcedure

	; Performs tree node "renaming". Pointers to a Name$/NewName$ element are no more valid after this operation, because map item is deleted and replaced by new one
	; Name$				tree item identifier to rename
	; NewName$			new item name
	; RETURN:			true on success, false else
	Procedure TreeRename (Map Tree.TreeNode(), Name$, NewName$)
		If Name$ = "" Or NewName$ = "" Or FindMapElement(Tree(), Name$) = 0 Or FindMapElement(Tree(), NewName$)
			; element not exists, or element with newname$ already exists
			ProcedureReturn #False
		EndIf
		Protected tKey$
		ForEach Tree(Name$)\Childs$()
			Tree(Tree(Name$)\Childs$())\Parent$ = NewName$
		Next
		Protected Parent$ = Tree(Name$)\Parent$
		If Parent$
			ForEach tree(Parent$)\Childs$()
				If Tree(Parent$)\Childs$() = Name$
					Tree(Parent$)\Childs$() = NewName$
					Break
				EndIf
			Next
		EndIf
		CopyStructure(@Tree(Name$), @Tree(NewName$), TreeNode)
		Tree(NewName$)\Name$ = NewName$
		DeleteMapElement(Tree(), Name$)
		ProcedureReturn #True
	EndProcedure

	; A function to examine tree data
	; StartName$		tree item identifier to start from it
	; *Callback			pointer to a function which will be called from here. Function must accept 1 argument: pointer to a Tree() element
	; GoUp				by default TreeRecurse() moves from current item to it childs, if GoUp = #True, it will move from current to it's parents
	; MaxLevels			how deep it should go? -1 means infinite, 1 will stop after current element, 2 will examine current element + all elements on next level, and so on
	; RETURN:			none
	Procedure TreeRecurse (Map Tree.TreeNode(), StartName$, *Callback, GoUp = #False, MaxLevels = -1)
		If StartName$ = "" Or FindMapElement(Tree(), StartName$) = 0
			; element not exists
			ProcedureReturn
		EndIf
		If *Callback And CallFunctionFast(*Callback, @Tree(StartName$))
			; stop recursion by user choise
			ProcedureReturn
		EndIf
		MaxLevels -1
		If MaxLevels <> 0 And FindMapElement(Tree(), StartName$)
			If GoUp
				TreeRecurse(Tree(), Tree(StartName$)\Parent$, *Callback, GoUp, MaxLevels)
			Else
				ForEach Tree(StartName$)\Childs$()
					TreeRecurse(Tree(), Tree(StartName$)\Childs$(), *Callback, GoUp, MaxLevels)
				Next
			EndIf
		EndIf
	EndProcedure

	; Returns full path of specified element
	; RETURN:			path string, or empty string if no such element found
	Procedure$ TreeGetPath (Map Tree.TreeNode(), Name$)
		Protected Path$, CName$ = Name$
		Repeat
			If CName$ And FindMapElement(Tree(), CName$)
				Path$ = CName$ + Path$
				CName$ = Tree()\Parent$
				If CName$
					Path$ = "\" + Path$
				EndIf
			Else
				Break
			EndIf
		ForEver
		ProcedureReturn Path$
	EndProcedure

	; Returns number representing level of specified element
	; RETURN:			item deepth level (number of parents item has), starting from 0. -1 returned if item doesn't exists
	Procedure TreeGetLevel (Map Tree.TreeNode(), Name$)
		Protected Level = -1
		Repeat
			If Name$ And FindMapElement(Tree(), Name$)
				Name$ = Tree()\Parent$
				Level + 1
			Else
				Break
			EndIf
		ForEver
		ProcedureReturn Level
	EndProcedure

	; Returns number of nested items that specified element has
	; RETURN:			0 if element doesn't exists or doesn't have childs, 1+ else
	Procedure TreeGetChildCount (Map Tree.TreeNode(), Name$)
		If Name$ = "" Or FindMapElement(Tree(), Name$) = 0
			; element not exists
			ProcedureReturn 0
		EndIf
		ProcedureReturn ListSize(Tree(Name$)\Childs$())
	EndProcedure

;}

;{ Declares }

	Declare TreeMenuRaise()
	Declare TreeAddNode(Type$)
	Declare TreeCutNode()
	Declare TreeCopyNode()
	Declare TreePasteNode()
	Declare TreeDeleteNode(IgnoreConfirm)
	Declare TreeEditInit()
	Declare TreeItemChangedCB()
	Declare TreeEditAllow(Allow = #False)
	Declare TreeEditStart (*pvdi.NMTVDISPINFO)
	Declare TreeEditEnd (*pvdi.NMTVDISPINFO)
	Declare TreeLogicCanExport()
	Declare TreeDragStart (*pvdi2.NMTREEVIEW)
	Declare TreeDrag(LParam)
	Declare TreeDragEnd()
	Declare WndTreeSetCaption(Text$)
	Declare RedrawTree(Map ExpandedItems(), SelectedItem$)
	Declare WndTreeMsg(Window, Message, wParam, lParam)
	Declare TreeSelectedWindow()
	Declare TreeSelectedObject()
	Declare$ TreeSelectedName()
	Declare TreeHItemToData (Control, hwnd)
	Declare TreeExpandedNames(Map Out())
	Declare WndViewSet(*CWindow.OBJECT)
	Declare WndViewGet()
	Declare WndViewAddFont(ObjName$, Name$, Size, Style)
	Declare WndViewAddColor(ObjName$, Color, Type = #PB_Gadget_FrontColor)
	Declare WndViewAddTip(ObjName$, Tip$)
	Declare WndParamsSet(*obj.OBJECT, Param$, Value$, Group$ = "")
	Declare WndParamsRedraw()
	Declare WndParamsSetTIP(Text$)
	Declare WndParamsList (*obj.OBJECT)
	Declare WndParamsSetCaption(Text$)
	Declare SetDefaultValues (*obj.OBJECT)
	Declare SetValue (*obj.OBJECT, Name$, Value$)
	Declare XMLSetState(hash$)
	Declare$ XMLGetState()
	Declare$ XMLHash()
;}

;{ App : Data }

	Global NewMap Tree.TreeNode()
	Global WndTree, WndTree_list	; treeview window + control
	Global WndView					; main (preview window)
	Global APP_TITLE$ = NStr("S~v{xpSrd~pyre%7L&9'9&9") + #PB_EDITOR_BUILDCOUNT + NStr("\")
	Global APP_TITLE_EX$ = NStr("")
;}

;{ App : Config }

	Structure CONFIG
		Export2Clipboard.a
		NoCaptions.a
		ConfirmChanges.a
		RunCode.a
		DPIPatch.a
		ConfirmDelete.a
		NoLoop.a
		IOLastDIR$
	EndStructure

	Global cfgPath$ = GetPathPart(ProgramFilename()) + GetFilePart(ProgramFilename(), #PB_FileSystem_NoExtension) + ".cfg"
	Global cfg.CONFIG
	;{ Config XML v2 }

		; *InData		a structure of type CONFIG (define it as needed)
		; Filename$		path to a config file
		; Format		TRUE to reformat XML before save
		; RETURN:		0 on success
		;				1 if XML-related error
		;				3 if file-related error
		Procedure SaveCfg (*inData.CONFIG, Filename$, Format = #False)
			Protected Res, TreeXML = CreateXML(#PB_Any, #PB_UTF8)
			If IsXML(TreeXML) And InsertXMLStructure(RootXMLNode(TreeXML), *inData, CONFIG)
				If Format
					FormatXML(TreeXML, #PB_XML_ReFormat | #PB_XML_WindowsNewline, 2)
				EndIf
				If Not SaveXML(TreeXML, Filename$, #PB_XML_NoDeclaration)
					Res = 3
				EndIf
			Else
				Res = 1
			EndIf
			If IsXML(TreeXML) : FreeXML(TreeXML) :EndIf
			ProcedureReturn Res
		EndProcedure

		; *OutData		a structure of type CONFIG to receive data
		; Filename$		path to a config file
		; RETURN:		0 on success
		;				1 if XML-related error
		;				3 if file-related error
		Procedure ReadCfg (*OutData.CONFIG, Filename$)
			Protected Res, TreeXML
			TreeXML = LoadXML(#PB_Any, Filename$, #PB_UTF8)
			If IsXML(TreeXML)
				ExtractXMLStructure(MainXMLNode(TreeXML), *OutData, CONFIG) ; finally extract data
			Else
				Res = 1
			EndIf
			If IsXML(TreeXML) : FreeXML(TreeXML) : EndIf
			ProcedureReturn Res
		EndProcedure

	;}

;}

;{ Object	: functions	}

	Structure OBJPROPERTYLIST
		List values_list$()				; a list of all attributes supported by object
		Map  values_map.a()				; same list, but hashtable optimized for search
	EndStructure
	Structure OBJEVENTLIST
		List values_list$()				; list of events supported ("#PB_EventType_LeftClick", and so on).
	EndStructure
	Structure OBJFLAGLIST
		List values_list$()				; list of values possible for flag property pointed by flags() map
		Map  values_map.a()				; same data, hashtable
	EndStructure
	Structure OBJDATA
		properties.OBJPROPERTYLIST
		events.OBJEVENTLIST
		Map flags.OBJFLAGLIST()			; hashtable of object "predefined flags"-properties (there can be flags itself, but also others)
	EndStructure

	Global NewMap ObjProperties.OBJDATA()
	; Fills ObjProperties() with data for further use
	; This procedure defines almost everything of object data
	; RETURN:			none
	; ! Synchronize with object list (#C_ constants) and docs/features
	Procedure SetSupportedValues()
		Macro P (ParamName)
			AddElement(ObjProperties()\properties\values_list$())
			ObjProperties()\properties\values_list$() = ParamName
			ObjProperties()\properties\values_map(ParamName) = #True
		EndMacro

		Macro DQ
			"
		EndMacro
		Macro E(e)
			AddElement(ObjProperties()\events\values_list$())
			ObjProperties()\events\values_list$() = DQ#E#DQ
		EndMacro

		Macro F(flag, value)
			AddElement(ObjProperties()\flags(flag)\values_list$())
			ObjProperties()\flags(flag)\values_list$() = value
			ObjProperties()\flags(flag)\values_map(value) = #True
		EndMacro

		ObjProperties(#C_WINDOW)
		P(#v_name) : P(#v_id) : P(#v_text) : P(#v_height) : P(#v_width) : P(#v_flags) : P(#v_minwidth) : P(#v_maxwidth) : P(#v_minheight) : P(#v_maxheight)
		F(#v_minwidth, "auto") : F(#v_maxwidth, "auto") : F(#v_minheight, "auto") : F(#v_maxheight, "auto")
		F(#v_flags, "#PB_Window_SystemMenu") : F(#v_flags, "#PB_Window_MinimizeGadget") : F(#v_flags, "#PB_Window_SizeGadget") : F(#v_flags, "#PB_Window_MaximizeGadget") : F(#v_flags, "#PB_Window_Invisible") : F(#v_flags, "#PB_Window_TitleBar") : F(#v_flags, "#PB_Window_Tool") : F(#v_flags, "#PB_Window_BorderLess") : F(#v_flags, "#PB_Window_ScreenCentered") : F(#v_flags, "#PB_Window_WindowCentered") : F(#v_flags, "#PB_Window_Maximize") : F(#v_flags, "#PB_Window_Minimize") : F(#v_flags, "#PB_Window_NoActivate"); : F(#v_flags, "#PB_Window_NoGadgets")
		E(#True) : E(#PB_Event_Menu) : E(#PB_Event_Gadget) : E(#PB_Event_SysTray) : E(#PB_Event_Timer) : E(#PB_Event_CloseWindow) : E(#PB_Event_Repaint) : E(#PB_Event_SizeWindow) : E(#PB_Event_MoveWindow) : E(#PB_Event_MinimizeWindow) : E(#PB_Event_MaximizeWindow) : E(#PB_Event_RestoreWindow) : E(#PB_Event_ActivateWindow) : E(#PB_Event_DeactivateWindow) : E(#PB_Event_WindowDrop) : E(#PB_Event_GadgetDrop) : E(#PB_Event_RightClick) : E(#PB_Event_LeftClick) : E(#PB_Event_LeftDoubleClick)
		ObjProperties(#C_HBOX)
		P(#v_name) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_align) : P(#v_expand) : P(#v_spacing)
		F(#v_align, "top/left") : F(#v_align, "top/right") : F(#v_align, "bottom/left") : F(#v_align, "bottom/right") : F(#v_align, "center")
		F(#v_expand, "yes") : F(#v_expand, "no") : F(#v_expand, "equal"); : F(#v_expand, "item:")
		ObjProperties(#C_VBOX)
		P(#v_name) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_align) : P(#v_expand) : P(#v_spacing)
		F(#v_align, "top/left") : F(#v_align, "top/right") : F(#v_align, "bottom/left") : F(#v_align, "bottom/right") : F(#v_align, "center")
		F(#v_expand, "yes") : F(#v_expand, "no") : F(#v_expand, "equal"); : F(#v_expand, "item:")
		ObjProperties(#C_GRIDBOX)
		P(#v_name) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_columns) : P(#v_colspacing) : P(#v_rowspacing) : P(#v_colexpand) : P(#v_rowexpand)
		F(#v_colexpand, "yes") : F(#v_colexpand, "no") : F(#v_colexpand, "equal"); : F(#v_colexpand, "item:")
		F(#v_rowexpand, "yes") : F(#v_rowexpand, "no") : F(#v_rowexpand, "equal"); : F(#v_rowexpand, "item:")
		ObjProperties(#C_MULTIBOX)
		P(#v_name) : P(#v_id) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan): P(#v_tooltip)
		ObjProperties(#C_SINGLEBOX)
		P(#v_name) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_align) : P(#v_expand) : P(#v_expandheight) : P(#v_expandwidth) : P(#v_margin)
		F(#v_align, "top/left") : F(#v_align, "top/right") : F(#v_align, "bottom/left") : F(#v_align, "bottom/right") : F(#v_align, "center")
		F(#v_expand, "yes") : F(#v_expand, "no") : F(#v_expand, "vertical") : F(#v_expand, "horizontal")
		ObjProperties(#C_DIALOGS)
		P(#v_font)
		ObjProperties(#C_BUTTON)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True)
		F(#v_flags, "#PB_Button_Default") : F(#v_flags, "#PB_Button_Left") : F(#v_flags, "#PB_Button_MultiLine") : F(#v_flags, "#PB_Button_Right") : F(#v_flags, "#PB_Button_Toggle") : F(#v_onevent, "yes")
		ObjProperties(#C_BUTTONIMAGE)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True)
		F(#v_flags, "#PB_Button_Toggle") : F(#v_onevent, "yes")
		ObjProperties(#C_CALENDAR)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True)
		F(#v_flags, "#PB_Calendar_Borderless") : F(#v_onevent, "yes")
		ObjProperties(#C_CANVAS)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable)
		E(#True) : E(#PB_EventType_MouseEnter) : E(#PB_EventType_MouseLeave) : E(#PB_EventType_MouseMove) : E(#PB_EventType_MouseWheel) : E(#PB_EventType_LeftButtonDown) : E(#PB_EventType_LeftButtonUp) : E(#PB_EventType_LeftClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightButtonDown) : E(#PB_EventType_RightButtonUp) : E(#PB_EventType_RightClick) : E(#PB_EventType_RightDoubleClick) : E(#PB_EventType_MiddleButtonDown) : E(#PB_EventType_MiddleButtonUp) : E(#PB_EventType_Focus) : E(#PB_EventType_LostFocus) : E(#PB_EventType_KeyDown) : E(#PB_EventType_KeyUp) : E(#PB_EventType_Input)
		F(#v_flags, "#PB_Canvas_Border") : F(#v_flags, "#PB_Canvas_ClipMouse") : F(#v_flags, "#PB_Canvas_Keyboard") : F(#v_flags, "#PB_Canvas_DrawFocus") : F(#v_onevent, "yes")
		ObjProperties(#C_CHECKBOX)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True)
		F(#v_flags, "#PB_CheckBox_Right") : F(#v_flags, "#PB_CheckBox_Center") : F(#v_flags, "#PB_CheckBox_ThreeState") : F(#v_onevent, "yes")
		ObjProperties(#C_COMBOBOX)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_Focus) : E(#PB_EventType_LostFocus)
		F(#v_flags, "#PB_ComboBox_Editable") : F(#v_flags, "#PB_ComboBox_LowerCase") : F(#v_flags, "#PB_ComboBox_UpperCase") : F(#v_flags, "#PB_ComboBox_Image") : F(#v_onevent, "yes")
		ObjProperties(#C_CONTAINER)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags): P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor)
		E(#True) : E(#PB_EventType_Resize)
		F(#v_flags, "#PB_Container_BorderLess") : F(#v_flags, "#PB_Container_Flat") : F(#v_flags, "#PB_Container_Raised") : F(#v_flags, "#PB_Container_Single") : F(#v_flags, "#PB_Container_Double") : F(#v_onevent, "yes")
		ObjProperties(#C_DATE)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_Change)
		F(#v_flags, "#PB_Date_UpDown") : F(#v_onevent, "yes")
		ObjProperties(#C_EDITOR)
		P(#v_name) : P(#v_id) : P(#v_text) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_Focus) : E(#PB_EventType_LostFocus)
		F(#v_flags, "#PB_Editor_ReadOnly") : F(#v_flags, "#PB_Editor_WordWrap") : F(#v_onevent, "yes")
		ObjProperties(#C_EMPTY)
		P(#v_name) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan)
		ObjProperties(#C_EXPLORERCOMBO)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent) : P(#v_variable) : P(#v_font)
		E(#True)
		F(#v_flags, "#PB_Explorer_DrivesOnly") : F(#v_flags, "#PB_Explorer_Editable") : F(#v_flags, "#PB_Explorer_NoMyDocuments") : F(#v_onevent, "yes")
		ObjProperties(#C_EXPLORERLIST)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_LeftClick) : E(#PB_EventType_RightClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightDoubleClick) : E(#PB_EventType_DragStart)
		F(#v_flags, "#PB_Explorer_BorderLess") : F(#v_flags, "#PB_Explorer_AlwaysShowSelection"): F(#v_flags, "#PB_Explorer_MultiSelect") : F(#v_flags, "#PB_Explorer_GridLines") : F(#v_flags, "#PB_Explorer_HeaderDragDrop"): F(#v_flags, "#PB_Explorer_FullRowSelect") : F(#v_flags, "#PB_Explorer_NoFolders"): F(#v_flags, "#PB_Explorer_NoParentFolder"): F(#v_flags, "#PB_Explorer_NoSort") : F(#v_flags, "#PB_Explorer_NoDriveRequester") : F(#v_flags, "#PB_Explorer_NoMyDocuments") : F(#v_flags, "#PB_Explorer_AutoSort") : F(#v_flags, "#PB_Explorer_HiddenFiles") : F(#v_onevent, "yes")
		ObjProperties(#C_EXPLORERTREE)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_LeftClick) : E(#PB_EventType_RightClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightDoubleClick) : E(#PB_EventType_DragStart)
		F(#v_flags, "#PB_Explorer_BorderLess") : F(#v_flags, "#PB_Explorer_AlwaysShowSelection") : F(#v_flags, "#PB_Explorer_NoLines") : F(#v_flags, "#PB_Explorer_NoButtons") : F(#v_flags, "#PB_Explorer_NoFiles") : F(#v_flags, "#PB_Explorer_NoDriveRequester") : F(#v_flags, "#PB_Explorer_NoMyDocuments") : F(#v_flags, "#PB_Explorer_AutoSort") : F(#v_onevent, "yes")
		ObjProperties(#C_FRAME)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_text) : P(#v_flags) : P(#v_variable) : P(#v_font)
		F(#v_flags, "#PB_Frame_Single") : F(#v_flags, "#PB_Frame_Double") : F(#v_flags, "#PB_Frame_Flat")
		ObjProperties(#C_HYPERLINK)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_LeftClick)
		F(#v_flags, "#PB_Hyperlink_Underline") : F(#v_onevent, "yes")
		ObjProperties(#C_IMAGE)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True) : E(#PB_EventType_LeftClick) : E(#PB_EventType_RightClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightDoubleClick) : E(#PB_EventType_DragStart)
		F(#v_flags, "#PB_Image_Border") : F(#v_flags, "#PB_Image_Raised") : F(#v_onevent, "yes")
		ObjProperties(#C_IPADDRESS)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True)
		F(#v_onevent, "yes")
		ObjProperties(#C_LISTICON)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_LeftClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightClick) : E(#PB_EventType_RightDoubleClick) : E(#PB_EventType_Change) : E(#PB_EventType_DragStart)
		F(#v_flags, "#PB_ListIcon_CheckBoxes") : F(#v_flags, "#PB_ListIcon_ThreeState") : F(#v_flags, "#PB_ListIcon_MultiSelect") : F(#v_flags, "#PB_ListIcon_GridLines") : F(#v_flags, "#PB_ListIcon_FullRowSelect") : F(#v_flags, "#PB_ListIcon_HeaderDragDrop") : F(#v_flags, "#PB_ListIcon_AlwaysShowSelection") : F(#v_onevent, "yes")
		ObjProperties(#C_LISTVIEW)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_LeftClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightClick)
		F(#v_flags, "#PB_ListView_Multiselect") : F(#v_flags, "#PB_ListView_ClickSelect") : F(#v_onevent, "yes")
		ObjProperties(#C_OPTION)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent) : P(#v_group): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True)
		F(#v_onevent, "yes")
		ObjProperties(#C_PANEL)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_Resize)
		F(#v_onevent, "yes")
		ObjProperties(#C_PROGRESSBAR)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_max) : P(#v_min) : P(#v_value) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		F(#v_flags, "#PB_ProgressBar_Smooth") : F(#v_flags, "#PB_ProgressBar_Vertical")
		ObjProperties(#C_SCINTILLA)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags); : P(#v_onevent) : P(#v_variable) : P(#v_font)
		E(#True)
		ObjProperties(#C_SCROLLAREA)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent) : P(#v_scrolling) : P(#v_innerheight) : P(#v_innerwidth) : P(#v_step): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor)
		E(#True) : E(#PB_EventType_Resize)
		F(#v_flags, "#PB_ScrollArea_Flat") : F(#v_flags, "#PB_ScrollArea_Raised") : F(#v_flags, "#PB_ScrollArea_Single") : F(#v_flags, "#PB_ScrollArea_BorderLess") : F(#v_flags, "#PB_ScrollArea_Center")
		F(#v_scrolling, "vertical") : F(#v_scrolling, "horizontal") : F(#v_scrolling, "both")
		F(#v_innerheight, "auto") : F(#v_innerwidth, "auto")
		F(#v_onevent, "yes")
		ObjProperties(#C_SCROLLBAR)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_max) : P(#v_min) : P(#v_value) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent) : P(#v_page): P(#v_tooltip) : P(#v_variable)
		E(#True)
		F(#v_flags, "#PB_ScrollBar_Vertical") : F(#v_onevent, "yes")
		ObjProperties(#C_SPIN)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_max) : P(#v_min) : P(#v_value) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_Up) : E(#PB_EventType_Down)
		F(#v_flags, "#PB_Spin_ReadOnly") : F(#v_flags, "#PB_Spin_Numeric") : F(#v_onevent, "yes")
		ObjProperties(#C_SPLITTER)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_firstmin) : P(#v_secondmin): P(#v_tooltip) : P(#v_variable)
		F(#v_flags, "#PB_Splitter_Vertical") : F(#v_flags, "#PB_Splitter_Separator") : F(#v_flags, "#PB_Splitter_FirstFixed") : F(#v_flags, "#PB_Splitter_SecondFixed")
		F(#v_firstmin, "auto") : F(#v_secondmin, "auto")
		ObjProperties(#C_STRING)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_Change) : E(#PB_EventType_Focus) : E(#PB_EventType_LostFocus)
		F(#v_flags, "#PB_String_Numeric") : F(#v_flags, "#PB_String_Password") : F(#v_flags, "#PB_String_ReadOnly") : F(#v_flags, "#PB_String_LowerCase") : F(#v_flags, "#PB_String_UpperCase") : F(#v_flags, "#PB_String_BorderLess") : F(#v_onevent, "yes")
		ObjProperties(#C_TAB)
		P(#v_name) : P(#v_height) : P(#v_width) : P(#v_text)
		ObjProperties(#C_TEXT)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_text) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		F(#v_flags, "#PB_Text_Center") : F(#v_flags, "#PB_Text_Right") : F(#v_flags, "#PB_Text_Border")
		ObjProperties(#C_TRACKBAR)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_max) : P(#v_min) : P(#v_value) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable)
		E(#True)
		F(#v_flags, "#PB_TrackBar_Ticks") : F(#v_flags, "#PB_TrackBar_Vertical") : F(#v_onevent, "yes")
		ObjProperties(#C_TREE)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_onevent): P(#v_tooltip) : P(#v_variable) : P(#v_backcolor) : P(#v_forecolor) : P(#v_font)
		E(#True) : E(#PB_EventType_LeftClick) : E(#PB_EventType_LeftDoubleClick) : E(#PB_EventType_RightClick) : E(#PB_EventType_RightDoubleClick) : E(#PB_EventType_Change) : E(#PB_EventType_DragStart)
		F(#v_flags, "#PB_Tree_AlwaysShowSelection") : F(#v_flags, "#PB_Tree_NoLines") : F(#v_flags, "#PB_Tree_NoButtons") : F(#v_flags, "#PB_Tree_CheckBoxes") : F(#v_flags, "#PB_Tree_ThreeState") : F(#v_onevent, "yes")
		ObjProperties(#C_WEB)
		P(#v_name) : P(#v_id) : P(#v_disabled) : P(#v_invisible) : P(#v_height) : P(#v_width) : P(#v_rowspan) : P(#v_colspan) : P(#v_flags) : P(#v_variable)
		E(#PB_EventType_TitleChange) : E(#PB_EventType_StatusChange) : E(#PB_EventType_DownloadStart) : E(#PB_EventType_DownloadProgress) : E(#PB_EventType_DownloadEnd) : E(#PB_EventType_PopupWindow) : E(#PB_EventType_PopupMenu)
		UndefineMacro F
		UndefineMacro P
		UndefineMacro E
		UndefineMacro DQ
	EndProcedure

	SetSupportedValues()
	; Add new object to Tree
	; Parent$		parent object name
	; Type			one of xml UI types or controls
	; *objData		a data to attach. if 0, SetDefaultValues() is called instead.  "treeName$, type$, name$" properties of *objData are ignored
	; RETURN:		pointer to created object on success
	Procedure objNew (Parent$, Type$, Name$ = "", *objData.OBJECT = 0, Order = #PB_List_First, CopyOnExists = #False)
		Protected *New.OBJECT
		Protected t = 1
		Protected treeName$
		Protected objName$
		If Name$
			objName$ = Name$
			treeName$ = LCase(objName$)
			If FindMapElement(Tree(), treeName$)
				If Not CopyOnExists
					ProcedureReturn 0
				EndIf
				Name$ = Name$ + "_c"
				objName$ = Name$ + Str(t)
				treeName$ = LCase(objName$)
			EndIf
			While FindMapElement(Tree(), treeName$)
				t + 1
				objName$ = Name$ + Str(t)
				treeName$ = LCase(objName$)
			Wend
		Else
			objName$ = type$ + Str(t)
			treeName$ = LCase(objName$)
			If FindMapElement(Tree(), treeName$) And Not CopyOnExists
				ProcedureReturn 0
			EndIf
			While FindMapElement(Tree(), treeName$)
				t + 1
				objName$ = type$ + Str(t)
				treeName$ = LCase(objName$)
			Wend
		EndIf
		If TreeAdd(Tree(), treeName$, 0, Parent$, Order)
			*New = @Tree(treeName$)\Data
			If *objData
				CopyStructure(*objData, *New, OBJECT)
			EndIf
			*New\treeName$ = treeName$
			*New\pEXTRA\Type$ = type$
			*New\pCOMMON\name$ = objName$
			If Not *objData
				SetDefaultValues (*New)
 			EndIf
		EndIf
		ProcedureReturn *New
	EndProcedure

	; Updates value of given object. Value is referenced by it's text name
	; RETURN:		none
	; !! Synchronize with: SetValue, GetValue, GetInfo, SetDefaultValues, SetSupportedValues, GetDefaultValue
	Procedure SetValue (*obj.OBJECT, Name$, Value$)
		Protected isEx
		Select Name$
			Case #v_name: 				*obj\pCOMMON\name$ = Value$
			Case #v_id: 				*obj\pCOMMON\id$ = Value$
			Case #v_disabled:			*obj\pCOMMON\disabled$ = Value$
			Case #v_invisible:			*obj\pCOMMON\invisible$ = Value$
			Case #v_height: 			*obj\pCOMMON\height$ = Value$
			Case #v_width: 				*obj\pCOMMON\width$ = Value$
			Case #v_max:				*obj\pCOMMON\max$ = Value$
			Case #v_min:				*obj\pCOMMON\min$ = Value$
			Case #v_rowspan: 			*obj\pCOMMON\rowspan$ = Value$
			Case #v_colspan: 			*obj\pCOMMON\colspan$ = Value$
			Case #v_flags: 				*obj\pCOMMON\flags$ = Value$
			Case #v_text: 				*obj\pCOMMON\text$ = Value$
			Case #v_value: 				*obj\pCOMMON\value$ = Value$
			Case #v_tooltip:			*obj\pCOMMON\tooltip$ = Value$
			Case #v_font:				*obj\pCOMMON\font$ = Value$
			Case #v_forecolor:			*obj\pCOMMON\forecolor$ = Value$
			Case #v_backcolor:			*obj\pCOMMON\backcolor$ = Value$
			Case #v_variable:			*obj\pCOMMON\variable$ = Value$
			Case #v_onevent: 			*obj\pEVENTS\onevent$ = Value$
			Default: isEx = #True
		EndSelect
		If isEx
			Select *obj\pEXTRA\Type$
				Case #C_WINDOW:
					Select Name$
						Case #v_minwidth: 			*obj\pEXTRA\window\minwidth$ = Value$
						Case #v_maxwidth: 			*obj\pEXTRA\window\maxwidth$ = Value$
						Case #v_minheight: 			*obj\pEXTRA\window\minheight$ = Value$
						Case #v_maxheight: 			*obj\pEXTRA\window\maxheight$ = Value$
					EndSelect
				Case #C_HBOX, #C_VBOX:
					Select Name$
						Case #v_align: 				*obj\pEXTRA\hvbox\align$ = Value$
						Case #v_expand: 			*obj\pEXTRA\hvbox\expand$ = Value$
						Case #v_spacing: 			*obj\pEXTRA\hvbox\spacing$ = Value$
					EndSelect
				Case #C_GRIDBOX:
					Select Name$
						Case #v_colexpand: 			*obj\pEXTRA\gridbox\colexpand$ = Value$
						Case #v_colspacing: 		*obj\pEXTRA\gridbox\colspacing$ = Value$
						Case #v_columns: 			*obj\pEXTRA\gridbox\columns$ = Value$
						Case #v_rowexpand: 			*obj\pEXTRA\gridbox\rowexpand$ = Value$
						Case #v_rowspacing: 		*obj\pEXTRA\gridbox\rowspacing$ = Value$
					EndSelect
				Case #C_MULTIBOX:
				Case #C_SINGLEBOX:
					Select Name$
						Case #v_align: 				*obj\pEXTRA\singlebox\align$ = Value$
						Case #v_expand: 			*obj\pEXTRA\singlebox\expand$ = Value$
						Case #v_expandheight:	 	*obj\pEXTRA\singlebox\expandheight$ = Value$
						Case #v_expandwidth: 		*obj\pEXTRA\singlebox\expandwidth$ = Value$
						Case #v_margin: 			*obj\pEXTRA\singlebox\margin$ = Value$
					EndSelect
				Case #C_OPTION:
					Select Name$
						Case #v_group:				*obj\pEXTRA\option\group$ = Value$
					EndSelect
				Case #C_SCROLLAREA:
					Select Name$
						Case #v_scrolling:			*obj\pEXTRA\scrollarea\scrolling$ = Value$
						Case #v_innerheight:		*obj\pEXTRA\scrollarea\innerheight$ = Value$
						Case #v_innerwidth:			*obj\pEXTRA\scrollarea\innerwidth$ = Value$
						Case #v_step:				*obj\pEXTRA\scrollarea\scrollstep$ = Value$
					EndSelect
				Case #C_SCROLLBAR:
					Select Name$
						Case #v_page:				*obj\pEXTRA\scrollbar\page$ = Value$
					EndSelect
				Case #C_SPLITTER:
					Select Name$
						Case #v_firstmin:			*obj\pEXTRA\splitter\firstmin$ = Value$
						Case #v_secondmin:			*obj\pEXTRA\splitter\secondmin$ = Value$
					EndSelect
				Default: 	; unknown/unsupported attribute
					Debug "SetValue: unknown " + Name$
			EndSelect
		EndIf
	EndProcedure

	; Returns value of given object. Value is referenced by it's text name
	; RETURN:		string
	; !! Synchronize with: SetValue, GetValue, GetInfo, SetDefaultValues, SetSupportedValues, GetDefaultValue
	Procedure$ GetValue (*obj.OBJECT, Name$)
		Protected Res$
		Protected isEx
		Select Name$
			Case #v_name: 					Res$ = *obj\pCOMMON\name$
			Case #v_id: 					Res$ = *obj\pCOMMON\id$
			Case #v_disabled:				Res$ = *obj\pCOMMON\disabled$
			Case #v_invisible:				Res$ = *obj\pCOMMON\invisible$
			Case #v_height: 				Res$ = *obj\pCOMMON\height$
			Case #v_width: 					Res$ = *obj\pCOMMON\width$
			Case #v_max:					Res$ = *obj\pCOMMON\max$
			Case #v_min:					Res$ = *obj\pCOMMON\min$
			Case #v_rowspan: 				Res$ = *obj\pCOMMON\rowspan$
			Case #v_colspan: 				Res$ = *obj\pCOMMON\colspan$
			Case #v_flags: 					Res$ = *obj\pCOMMON\flags$
			Case #v_text: 					Res$ = *obj\pCOMMON\text$
			Case #v_value: 					Res$ = *obj\pCOMMON\value$
			Case #v_tooltip:				Res$ = *obj\pCOMMON\tooltip$
			Case #v_font:					Res$ = *obj\pCOMMON\font$
			Case #v_forecolor:				Res$ = *obj\pCOMMON\forecolor$
			Case #v_backcolor:				Res$ = *obj\pCOMMON\backcolor$
			Case #v_variable:				Res$ = *obj\pCOMMON\variable$
			Case #v_onevent: 				Res$ = *obj\pEVENTS\onevent$
			Default: isEx = #True
		EndSelect
		If isEx
		Select *obj\pEXTRA\Type$
			Case #C_WINDOW:
				Select Name$
					Case #v_minwidth: 		Res$ = *obj\pEXTRA\window\minwidth$
					Case #v_maxwidth: 		Res$ = *obj\pEXTRA\window\maxwidth$
					Case #v_minheight: 		Res$ = *obj\pEXTRA\window\minheight$
					Case #v_maxheight: 		Res$ = *obj\pEXTRA\window\maxheight$
				EndSelect
			Case #C_HBOX, #C_VBOX:
				Select Name$
					Case #v_align: 			Res$ = *obj\pEXTRA\hvbox\align$
					Case #v_expand: 		Res$ = *obj\pEXTRA\hvbox\expand$
					Case #v_spacing: 		Res$ = *obj\pEXTRA\hvbox\spacing$
				EndSelect
			Case #C_GRIDBOX:
				Select Name$
					Case #v_colexpand: 		Res$ = *obj\pEXTRA\gridbox\colexpand$
					Case #v_colspacing: 	Res$ = *obj\pEXTRA\gridbox\colspacing$
					Case #v_columns: 		Res$ = *obj\pEXTRA\gridbox\columns$
					Case #v_rowexpand: 		Res$ = *obj\pEXTRA\gridbox\rowexpand$
					Case #v_rowspacing: 	Res$ = *obj\pEXTRA\gridbox\rowspacing$
				EndSelect
			Case #C_MULTIBOX:
			Case #C_SINGLEBOX:
				Select Name$
					Case #v_align: 			Res$ = *obj\pEXTRA\singlebox\align$
					Case #v_expand: 		Res$ = *obj\pEXTRA\singlebox\expand$
					Case #v_expandheight:	Res$ = *obj\pEXTRA\singlebox\expandheight$
					Case #v_expandwidth: 	Res$ = *obj\pEXTRA\singlebox\expandwidth$
					Case #v_margin: 		Res$ = *obj\pEXTRA\singlebox\margin$
				EndSelect
			Case #C_OPTION:
				Select Name$
					Case #v_group:			Res$ = *obj\pEXTRA\option\group$
				EndSelect
			Case #C_SCROLLAREA:
				Select Name$
					Case #v_scrolling:		Res$ = *obj\pEXTRA\scrollarea\scrolling$
					Case #v_innerheight:	Res$ = *obj\pEXTRA\scrollarea\innerheight$
					Case #v_innerwidth:		Res$ = *obj\pEXTRA\scrollarea\innerwidth$
					Case #v_step:			Res$ = *obj\pEXTRA\scrollarea\scrollstep$
				EndSelect
			Case #C_SCROLLBAR:
				Select Name$
					Case #v_page:			Res$ = *obj\pEXTRA\scrollbar\page$
				EndSelect
			Case #C_SPLITTER:
				Select Name$
					Case #v_firstmin:		Res$ = *obj\pEXTRA\splitter\firstmin$
					Case #v_secondmin:		Res$ = *obj\pEXTRA\splitter\secondmin$
				EndSelect
			Default:
				Debug "GetValue: unknown property " + Name$
		EndSelect
		EndIf
		ProcedureReturn Res$
	EndProcedure

	; Returns default value of given object property
	; 'Default' here means value which is default for dialogs library, not something default defined by designer
	; So don't confuse this proc with SetDefaultValues(), they are different
	; RETURN:		string
	; !! Synchronize with: SetValue, GetValue, GetInfo, SetDefaultValues, SetSupportedValues, GetDefaultValue
	Procedure$ GetDefaultValue (Objtype$, Name$)
		Protected Res$
		Protected isEx
		Select Name$
			Case #v_name: 					Res$ = ""
			Case #v_id: 					Res$ = ""
			Case #v_disabled:				Res$ = "no"
			Case #v_invisible:				Res$ = "no"
			Case #v_height: 				Res$ = "0"
			Case #v_width: 					Res$ = "0"
			Case #v_max:					Res$ = "0"
			Case #v_min:					Res$ = "0"
			Case #v_rowspan: 				Res$ = ""
			Case #v_colspan: 				Res$ = ""
			Case #v_flags: 					Res$ = ""
			Case #v_text: 					Res$ = ""
			Case #v_value: 					Res$ = "0"
			Case #v_tooltip:				Res$ = ""
			Case #v_font:					Res$ = ""
			Case #v_forecolor:				Res$ = ""
			Case #v_backcolor:				Res$ = ""
			Case #v_variable:				Res$ = "no"
			Case #v_onevent: 				Res$ = "no"
			Default: isEx = #True
		EndSelect
		If isEx
			Select Objtype$
				Case #C_WINDOW:
					Select Name$
						Case #v_minwidth: 		Res$ = "0"
						Case #v_maxwidth: 		Res$ = "0"
						Case #v_minheight: 		Res$ = "0"
						Case #v_maxheight: 		Res$ = "0"
					EndSelect
				Case #C_HBOX, #C_VBOX:
					Select Name$
						Case #v_align: 			Res$ = ""
						Case #v_expand: 		Res$ = ""
						Case #v_spacing: 		Res$ = ""
					EndSelect
				Case #C_GRIDBOX:
					Select Name$
						Case #v_colexpand: 		Res$ = ""
						Case #v_colspacing: 	Res$ = ""
						Case #v_columns: 		Res$ = ""
						Case #v_rowexpand: 		Res$ = ""
						Case #v_rowspacing: 	Res$ = ""
					EndSelect
				Case #C_MULTIBOX:
				Case #C_SINGLEBOX:
					Select Name$
						Case #v_align: 			Res$ = ""
						Case #v_expand: 		Res$ = ""
						Case #v_expandheight:	Res$ = "0"
						Case #v_expandwidth: 	Res$ = "0"
						Case #v_margin: 		Res$ = ""
					EndSelect
				Case #C_OPTION:
					Select Name$
						Case #v_group:			Res$ = "0"
					EndSelect
				Case #C_SCROLLAREA:
					Select Name$
						Case #v_scrolling:		Res$ = ""
						Case #v_innerheight:	Res$ = ""
						Case #v_innerwidth:		Res$ = ""
						Case #v_step:			Res$ = ""
					EndSelect
				Case #C_SCROLLBAR:
					Select Name$
						Case #v_page:			Res$ = ""
					EndSelect
				Case #C_SPLITTER:
					Select Name$
						Case #v_firstmin:		Res$ = ""
						Case #v_secondmin:		Res$ = ""
					EndSelect
				Default: Debug "GetDefaultValue: unknown object " + Objtype$
			EndSelect
		EndIf
		ProcedureReturn Res$
	EndProcedure

	; Returns description of given object attribute
	; RETURN:		string with tooltip or empty string
	; ! Synchronize with properties list (#v_ constants)
	Procedure$ GetInfo (Objtype$, Name$)
		Protected Res$
		Select Name$
			Case #v_name: 					Res$ = "A string identifying the object name. Used with DialogGadget() function to get control identifier (ID property)"
			Case #v_id: 					Res$ = "#Number identifier of object. Can be a runtime constant. Is set to #PB_Any if empty"
			Case #v_disabled:				Res$ = "Creates the object disabled"
			Case #v_invisible:				Res$ = "Creates the object invisible"
			Case #v_height: 				Res$ = "Defines 'minimum size' of a control, can be '0'"
			Case #v_width: 					Res$ = "Defines 'minimum size' of a control, can be '0'"
			Case #v_max:					Res$ = "Max value"
			Case #v_min:					Res$ = "Min value"
			Case #v_rowspan: 				Res$ = "Used inside 'gridbox' only, allows an element to span multiple rows"
			Case #v_colspan: 				Res$ = "Used inside 'gridbox' only, allows an element to span multiple columns"
			Case #v_flags: 					Res$ = "Specific set of flags for this object"
			Case #v_text: 					Res$ = "Control caption / default text"
			Case #v_value: 					Res$ = "Default value of control"
			Case #v_tooltip:				Res$ = "A tooltip for this control. Displayed on mouse hover (code)"
			Case #v_font:					Res$ = "A font used by this object (code)"
			Case #v_forecolor:				Res$ = "Default text color (code)"
			Case #v_backcolor:				Res$ = "Default background color (code)"
			Case #v_variable:				Res$ = "Force variable declaration for this control (code)"
			Case #v_class:					Res$ = "Object type"
			Case #v_parent:					Res$ = "A name of object parent"
			Case #v_childs:					Res$ = "Number of childs object has"
			Case #v_onevent: 				Res$ = "Generate event callback (code). Can be auto-generated (yes) or custom proc. name, also several controls may bind to the same proc."
			Default:
			Select Objtype$
				Case #C_WINDOW:
					Select Name$
						Case #v_minwidth: 			Res$ = "Min allowed window width. Can be empty, '0', 'auto' or set to fixed positive value"
						Case #v_maxwidth: 			Res$ = "Max allowed window width. Can be empty, '0', 'auto' or set to fixed positive value"
						Case #v_minheight: 			Res$ = "Min allowed window height. Can be empty, '0', 'auto' or set to fixed positive value"
						Case #v_maxheight: 			Res$ = "Max allowed window height. Can be empty, '0', 'auto' or set to fixed positive value"
					EndSelect
				Case #C_HBOX: #C_VBOX:
					Select Name$
						Case #v_align: 				Res$ = "Defines how childs are ordered inside this container"
						Case #v_expand: 			Res$ = "Automatically changes size of child controls to fit all free space. 'yes' - enable resizing for all childs (default), 'no' - disabled, 'equal' - force all childs to have same size, 'item:<number>' - resize only one specified child"
						Case #v_spacing: 			Res$ = "Space to add between the packed childs (pixels)"
					EndSelect
				Case #C_GRIDBOX:
					Select Name$
						Case #v_colexpand: 			Res$ = "Automatically changes size of child controls to fit all free space. 'yes' - enable resizing for all childs (default), 'no' - disabled, 'equal' - force all childs to have same size, 'item:<number>' - resize only one specified child"
						Case #v_colspacing: 		Res$ = "Free space between columns (pixels)"
						Case #v_columns: 			Res$ = "Number of columns inside this control"
						Case #v_rowexpand: 			Res$ = "Automatically changes size of child controls to fit all free space. 'yes' - enable resizing for all childs, 'no' - disabled (default), 'equal' - force all childs to have same size, 'item:<number>' - resize only one specified child"
						Case #v_rowspacing: 		Res$ = "Free space between rows (pixels)"
					EndSelect
				Case #C_MULTIBOX:
				Case #C_SINGLEBOX:
					Select Name$
						Case #v_align: 				Res$ = "Defines how child is aligned inside container"
						Case #v_expand: 			Res$ = "Automatically changes size of child control to fit all free space. 'yes' - enable resizing (default), 'no' - disabled, 'vertical' - expand height only, 'horizontal' - expand width only"
						Case #v_expandheight:	 	Res$ = "Max width used to expand the child item"
						Case #v_expandwidth: 		Res$ = "Max height used to expand the child item"
						Case #v_margin: 			Res$ = "Extra free space around the child control (pixels)"
					EndSelect
				Case #C_OPTION:
					Select Name$
						Case #v_group:				Res$ = "Items with same 'group' number have radio communication between them (what a joke :)"
					EndSelect
				Case #C_SCROLLAREA:
					Select Name$
						Case #v_scrolling:			Res$ = "Scrollbar: vertical,horizontal or both (default)"
						Case #v_innerheight:		Res$ = "Internal size of scroll area: integer value or 'auto' (default)"
						Case #v_innerwidth:			Res$ = "Internal size of scroll area: integer value or 'auto' (default)"
						Case #v_step:				Res$ = "Scrolling step: integer"
					EndSelect
				Case #C_SCROLLBAR:
					Select Name$
						Case #v_page:				Res$ = "Page length"
					EndSelect
				Case #C_SPLITTER:
					Select Name$
						Case #v_firstmin:			Res$ = "Sets the minimum size (in pixels) of the first child. Default is 'auto'"
						Case #v_secondmin:			Res$ = "Sets the minimum size (in pixels) of the second child. Default is 'auto'"
					EndSelect
				Default:
					Debug "Tooltip missing for " + Objtype$ + ":" + Name$
			EndSelect
		EndSelect
		ProcedureReturn Res$
	EndProcedure

	; Set all object properties to their default values
	; IMPORTANT:	object Name$ and Type$ values have to be set already!
	; RETURN:		none
	; !! Synchronize with: SetValue, GetValue, GetInfo, SetDefaultValues, SetSupportedValues, GetDefaultValue
	Procedure SetDefaultValues (*obj.OBJECT)
		Select *obj\pEXTRA\Type$
			Case #C_WINDOW:
				SetValue(*obj, #v_name, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_flags, 			"#PB_Window_SystemMenu")
				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
			Case #C_HBOX, #C_VBOX:
			Case #C_GRIDBOX:
			Case #C_MULTIBOX:
			Case #C_SINGLEBOX:
			Case #C_DIALOGS:
			Case #C_BUTTON:
				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_BUTTONIMAGE:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_CALENDAR:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_CANVAS:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_CHECKBOX:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_COMBOBOX
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_CONTAINER:
				SetValue(*obj, #v_variable, 		"yes")
			Case #C_DATE:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_EDITOR
				SetValue(*obj, #v_onevent, 			"yes")
 			Case #C_EMPTY:
			Case #C_EXPLORERCOMBO:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_EXPLORERLIST:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_EXPLORERTREE:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_FRAME:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_variable, 		"yes")
			Case #C_HYPERLINK:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_IMAGE:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_IPADDRESS:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_LISTICON:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_LISTVIEW:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_OPTION:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_PANEL:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_PROGRESSBAR:
				SetValue(*obj, #v_variable, 		"yes")
			Case #C_SCINTILLA:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_SCROLLAREA:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_SCROLLBAR:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_SPIN:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_SPLITTER:
				SetValue(*obj, #v_variable, 		"yes")
			Case #C_STRING:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_TAB:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_variable, 		"yes")
			Case #C_TEXT:
 				SetValue(*obj, #v_text, 			*obj\pCOMMON\name$)
				SetValue(*obj, #v_variable, 		"yes")
			Case #C_TRACKBAR:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_TREE:
				SetValue(*obj, #v_onevent, 			"yes")
			Case #C_WEB:
				SetValue(*obj, #v_variable, 		"yes")
			Default: 	; ???????????
				Debug "UNKNOWN OBJECT: " + *obj\pCOMMON\name$ + *obj\pEXTRA\Type$
		EndSelect
		If cfg\NoCaptions
			SetValue(*obj, #v_text, "")
		EndIf
	EndProcedure

	; Returns list of predefined values for specific control and property name
	; ObjType$			type of object
	; Param$			property name to get options/flags for
	; RETURN:			string of values separated with "|", or empty string
	; ; !! Uses data defined in SetSupportedValues()
	Procedure GetObjectFlagsList(Objtype$, Param$, List Out$())
		ClearList(Out$())
		If FindMapElement(ObjProperties(), Objtype$)
			If FindMapElement(ObjProperties()\flags(), Param$)
				CopyList(ObjProperties()\flags(Param$)\values_list$(), Out$())
			EndIf
		Else
			Debug "GetObjectFlagsList: unknown objtype " + Objtype$
		EndIf
	EndProcedure

	; Returns true if specified property is supported by given object type
	; ObjType$			type of object
	; Param$			name of flag property (flags, align, etc)
	; Value$			flag name
	; RETURN:			true or false
	; !! Uses data defined in SetSupportedValues()
	Procedure IsFlagSupported(ObjType$, Param$, Value$)
		If FindMapElement(ObjProperties(), Objtype$)
			If FindMapElement(ObjProperties()\flags(), Param$)
				If FindMapElement(ObjProperties()\flags(Param$)\values_map(), Value$)
					ProcedureReturn #True
				EndIf
			EndIf
		Else
			Debug "IsFlagSupported: unknown objtype " + Objtype$
			ProcedureReturn #False
		EndIf
	EndProcedure

	; Returns properties supported by object (list form)
	; RETURN:			Out$() data set on success
	; !! Uses data defined in SetSupportedValues()
	Procedure GetSupportedValuesList(Objtype$, List Out$())
		ClearList(Out$())
		If FindMapElement(ObjProperties(), Objtype$)
			CopyList(ObjProperties()\properties\values_list$(), Out$())
		Else
			Debug "GetSupportedValuesList: unknown objtype " + Objtype$
		EndIf
	EndProcedure

	; Returns true if specified property is supported by given object type
	; RETURN:			true or false
	; !! Uses data defined in SetSupportedValues()
	Procedure IsValueSupported(ObjType$, Param$)
		If FindMapElement(ObjProperties(), Objtype$)
			If FindMapElement(ObjProperties()\properties\values_map(), Param$)
				ProcedureReturn #True
			EndIf
		Else
			Debug "IsValueSupported: unknown objtype " + Objtype$
			ProcedureReturn #False
		EndIf
	EndProcedure

	; Returns true if given object type is container
	Procedure IsContainer (Objtype$)
		Select Objtype$
		Case #C_WINDOW, #C_HBOX, #C_VBOX, #C_GRIDBOX, #C_MULTIBOX, #C_SINGLEBOX, #C_CONTAINER, #C_FRAME, #C_PANEL, #C_SCROLLAREA, #C_SPLITTER, #C_TAB:
			ProcedureReturn #True
		Default:
			ProcedureReturn #False
		EndSelect
	EndProcedure

	; Returns list of event types supported by specified control
	; RETURN:			string of values separated with "|", or empty string
	; !! Uses data defined in SetSupportedValues()
	Procedure GetSupportedEventsList(Objtype$, List Out$())
		ClearList(Out$())
		If FindMapElement(ObjProperties(), Objtype$)
			CopyList(ObjProperties()\events\values_list$(), Out$())
			If FirstElement(Out$()) And Out$() = "#True"
				DeleteElement(Out$())
			EndIf
		Else
			Debug "GetSupportedEventsList: unknown objtype " + Objtype$
		EndIf
	EndProcedure

	; Check if object name is valid to use. Such check is required for objects used in events generation
	; RETURN:	true if string is clear, false if it doesn't (can't be used in PB code)
	Procedure isNameValid(Dat$)
		Protected t, l = Len(dat$)
		Protected c$
		Protected res = #True
		Protected CharsAllowedStart$ = "abcdefghijklmnopqrstuvwxyz_"
		Protected CharsAllowed$ = "abcdefghijklmnopqrstuvwxyz_0123456789"
		Protected MsgChars$
		Dat$ = LCase(Dat$)
		For T = 1 To L
			c$ = Mid(Dat$, T, 1)
			If T = 1
				If FindString(CharsAllowedStart$, c$) = 0
					MsgChars$ = "This object name must start with a-Z or _ character"
					res = #False
					Break
				EndIf
			Else
				If FindString(CharsAllowed$, c$) = 0
					MsgChars$ = "Only the following characters may pass:" + #CRLF$ + "a-z_A-Z_0-9"
					res = #False
					Break
				EndIf
			EndIf
		Next T
		If Not res
			MessageRequester("Invalid object name", MsgChars$, #PB_MessageRequester_Warning)
		EndIf
		ProcedureReturn res
	EndProcedure

	; Returns true if given type is known and valid, false else
	; !! Uses data defined in SetSupportedValues()
	Procedure isValidObject(ObjType$)
		If FindMapElement(ObjProperties(), Objtype$)
			ProcedureReturn #True
		Else
			Debug "isValidObject: unknown objtype " + Objtype$
			ProcedureReturn #False
		EndIf
	EndProcedure

;}

;{ XML		: generator	}

	Global XML_lastHash$			; this is used to track tree changes
	Global XML_current, XML_root	; global variables used on current generation
	Global NewMap XMLNodes()
	Global XML_preview				; is true if exporting for preview
	Global XML_preview_window$		; a name of window for current preview
	Global XML_exportcode			; is true if export code is performed
	Global XML_export				; this flag is set to true if export to XML file is performed
	Structure ITEMCALLBACK
		Name$
		Comment$
		CBWindow$
		*obj.OBJECT
	EndStructure
	Structure ITEMCALLBACKWINDOW
		List Callbacks.ITEMCALLBACK()
	EndStructure
	Structure ITEMDECLAREWINDOW
		Map Controls$()
	EndStructure
	Structure ITEMCODE
		Code$
	EndStructure
	Structure ITEMCODEWINDOW
		List Codes.ITEMCODE()
		List Variables$()
	EndStructure
	Structure ITEMFONTINFO
		Variable$
		FontName$
		FontSize.i
		FontStyle.i
	EndStructure

	Global NewMap XMLCodeWindows.ITEMCODEWINDOW()
	Global NewMap XMLFonts.ITEMFONTINFO()
	Global NewMap XMLCallbackWindows.ITEMCALLBACKWINDOW()
	Global NewMap XMLDeclaresWindows.ITEMDECLAREWINDOW()
	#SPECIAL_KEY		= "####DD2SPECIAL#####"
	Global DPIx, DPIy
	Global NewMap DPIv$()
	; Proc used by pCommonStr/pExtraStr generator procedures
	; It also applies DPI patch and fills DPIv$() map for future
	; Node			xml node
	; *obj			current object
	; Name$			name of attribute
	; Value$		value (pff)
	; RETURN:		none, node attribute may be modified
	Procedure AddAttribute(Node, *obj.OBJECT, Name$, Value$)
		#DPIAttributesX$ = "#v_width|#v_minwidth|#v_maxwidth|#v_colspacing|#v_expandwidth|#v_innerwidth"
		#DPIAttributesY$ = "#v_height|#v_minheight|#v_maxheight|#v_spacing|#v_rowspacing|#v_expandheight|#v_margin|#v_innerheight|#v_firstmin|#v_secondmin"
		If Value$ And IsValueSupported(*obj\pEXTRA\type$, Name$) And Not (Value$ = GetDefaultValue(*obj\pExtra\type$, Name$))
			If cfg\DPIPatch
				If XML_preview
					If FindString(#DPIAttributesX$, Name$ + "|") And isNumeric(Value$)
						SetXMLAttribute(Node, Name$, Str(ScaleX(Val(Value$))))
					ElseIf FindString(#DPIAttributesY$, Name$ + "|") And isNumeric(Value$)
						SetXMLAttribute(Node, Name$, Str(ScaleY(Val(Value$))))
					Else
						SetXMLAttribute(Node, Name$, Value$)
					EndIf
				ElseIf XML_exportcode
					If FindString(#DPIAttributesX$, Name$ + "|") And isNumeric(Value$)
						DPIx + 1
						DPIv$("DPIx" + Str(DPIx)) = "ScaleX(" + Value$ + ")"
						SetXMLAttribute(Node, Name$, "DPIx" + Str(DPIx))
					ElseIf FindString(#DPIAttributesY$, Name$ + "|") And isNumeric(Value$)
						DPIy + 1
						DPIv$("DPIy" + Str(DPIy)) = "ScaleY(" + Value$ + ")"
						SetXMLAttribute(Node, Name$, "DPIy" + Str(DPIy))
					Else
						SetXMLAttribute(Node, Name$, Value$)
					EndIf
				Else
					SetXMLAttribute(Node, Name$, Value$)
				EndIf
			Else
				SetXMLAttribute(Node, Name$, Value$)
			EndIf
		EndIf
	EndProcedure

	; Translates XML_COMMON_ATTRIBUTES and values to a node attributes
	; Attribute is only added if value is not empty
	; ParentWindow$		name of window item to which current object belongs
	; RETURN:			none
	Procedure pCommonStr (XmlNode, *obj.OBJECT, ParentWindow$, RowColSpan = #False)
		AddAttribute(XmlNode, *obj,  #v_name, 			*obj\pCOMMON\name$)
		AddAttribute(XmlNode, *obj,  #v_disabled, 		*obj\pCOMMON\disabled$)
		AddAttribute(XmlNode, *obj,  #v_flags, 			*obj\pCOMMON\flags$)
		AddAttribute(XmlNode, *obj,  #v_height, 		*obj\pCOMMON\height$)
		AddAttribute(XmlNode, *obj,  #v_invisible, 		*obj\pCOMMON\invisible$)
		AddAttribute(XmlNode, *obj,  #v_max, 			*obj\pCOMMON\max$)
		AddAttribute(XmlNode, *obj,  #v_min, 			*obj\pCOMMON\min$)
		AddAttribute(XmlNode, *obj,  #v_text, 			*obj\pCOMMON\text$)
		AddAttribute(XmlNode, *obj,  #v_value, 			*obj\pCOMMON\value$)
		AddAttribute(XmlNode, *obj,  #v_width, 			*obj\pCOMMON\width$)
		If XML_preview = #False
			AddAttribute(XmlNode, *obj,  #v_id,			*obj\pCOMMON\id$)
		EndIf
		If RowColSpan
			AddAttribute(XmlNode, *obj,  #v_rowspan, 	*obj\pCOMMON\rowspan$)
			AddAttribute(XmlNode, *obj,  #v_colspan, 	*obj\pCOMMON\colspan$)
		EndIf
		If XML_export
			If *obj\pCOMMON\forecolor$ And IsValueSupported(*obj\pEXTRA\type$, #v_forecolor) And Not (*obj\pCOMMON\forecolor$ = GetDefaultValue(*obj\pExtra\type$, #v_forecolor))
				SetXMLAttribute(XmlNode, #v_forecolor_wrap, *obj\pCOMMON\forecolor$)
			EndIf
			If *obj\pCOMMON\backcolor$ And IsValueSupported(*obj\pEXTRA\type$, #v_backcolor) And Not (*obj\pCOMMON\backcolor$ = GetDefaultValue(*obj\pExtra\type$, #v_backcolor))
				SetXMLAttribute(XmlNode, #v_backcolor_wrap, *obj\pCOMMON\backcolor$)
			EndIf
			If *obj\pCOMMON\font$ And IsValueSupported(*obj\pEXTRA\type$, #v_font) And Not (*obj\pCOMMON\font$ = GetDefaultValue(*obj\pExtra\type$, #v_font))
				SetXMLAttribute(XmlNode, #v_font_wrap, *obj\pCOMMON\font$)
			EndIf
			If *obj\pCOMMON\tooltip$ And IsValueSupported(*obj\pEXTRA\type$, #v_tooltip) And Not (*obj\pCOMMON\tooltip$ = GetDefaultValue(*obj\pExtra\type$, #v_tooltip))
				SetXMLAttribute(XmlNode, #v_tooltip_wrap, *obj\pCOMMON\tooltip$)
			EndIf
			If *obj\pCOMMON\variable$ And IsValueSupported(*obj\pEXTRA\type$, #v_variable) And Not (*obj\pCOMMON\variable$ = GetDefaultValue(*obj\pExtra\type$, #v_variable))
				SetXMLAttribute(XmlNode, #v_variable_wrap, *obj\pCOMMON\variable$)
			EndIf
		EndIf
		If XML_preview
			If *obj\pCOMMON\font$ And IsValueSupported(*obj\pEXTRA\type$, #v_font) And Not (*obj\pCOMMON\font$ = GetDefaultValue(*obj\pExtra\type$, #v_font))
				WndViewAddFont(*obj\pCOMMON\name$, StringField(*obj\pCOMMON\font$, 1, "|"), Val(StringField(*obj\pCOMMON\font$, 2, "|")), Val(StringField(*obj\pCOMMON\font$, 3, "|")))
			EndIf
			If *obj\pCOMMON\forecolor$ And IsValueSupported(*obj\pEXTRA\type$, #v_forecolor) And Not (*obj\pCOMMON\forecolor$ = GetDefaultValue(*obj\pExtra\type$, #v_forecolor))
				WndViewAddColor(*obj\pCOMMON\name$, Val(*obj\pCOMMON\forecolor$), #PB_Gadget_FrontColor)
			EndIf
			If *obj\pCOMMON\backcolor$ And IsValueSupported(*obj\pEXTRA\type$, #v_backcolor) And Not (*obj\pCOMMON\backcolor$ = GetDefaultValue(*obj\pExtra\type$, #v_backcolor))
				WndViewAddColor(*obj\pCOMMON\name$, Val(*obj\pCOMMON\backcolor$), #PB_Gadget_BackColor)
			EndIf
			If *obj\pCOMMON\tooltip$ And IsValueSupported(*obj\pEXTRA\type$, #v_tooltip) And Not (*obj\pCOMMON\tooltip$ = GetDefaultValue(*obj\pExtra\type$, #v_tooltip))
				WndViewAddTip(*obj\pCOMMON\name$, *obj\pCOMMON\tooltip$)
			EndIf
		EndIf
		If XML_exportcode
			If *obj\pCOMMON\forecolor$ And IsValueSupported(*obj\pEXTRA\type$, #v_forecolor) And Not (*obj\pCOMMON\forecolor$ = GetDefaultValue(*obj\pExtra\type$, #v_forecolor))
				AddElement(XMLCodeWindows(ParentWindow$)\Codes())
				With XMLCodeWindows(ParentWindow$)\Codes()
					\Code$ + "SetGadgetColor(" + *obj\pCOMMON\name$ + ~", #PB_Gadget_FrontColor, " + *obj\pCOMMON\forecolor$ + ~")"
				EndWith
				XMLDeclaresWindows(ParentWindow$)\Controls$(*obj\pCOMMON\name$) = *obj\pCOMMON\name$
			EndIf
			If *obj\pCOMMON\backcolor$ And IsValueSupported(*obj\pEXTRA\type$, #v_backcolor) And Not (*obj\pCOMMON\backcolor$ = GetDefaultValue(*obj\pExtra\type$, #v_backcolor))
				AddElement(XMLCodeWindows(ParentWindow$)\Codes())
				With XMLCodeWindows(ParentWindow$)\Codes()
					\Code$ + "SetGadgetColor(" + *obj\pCOMMON\name$ + ~", #PB_Gadget_BackColor, " + *obj\pCOMMON\backcolor$ + ~")"
				EndWith
				XMLDeclaresWindows(ParentWindow$)\Controls$(*obj\pCOMMON\name$) = *obj\pCOMMON\name$
			EndIf
			If *obj\pCOMMON\tooltip$ And IsValueSupported(*obj\pEXTRA\type$, #v_tooltip) And Not (*obj\pCOMMON\tooltip$ = GetDefaultValue(*obj\pExtra\type$, #v_tooltip))
				AddElement(XMLCodeWindows(ParentWindow$)\Codes())
				With XMLCodeWindows(ParentWindow$)\Codes()
					\Code$ + "GadgetToolTip(" + *obj\pCOMMON\name$ + ~", ~\"" + EscapeString(*obj\pCOMMON\tooltip$, #PB_String_EscapeInternal)  + ~"\")"
				EndWith
				XMLDeclaresWindows(ParentWindow$)\Controls$(*obj\pCOMMON\name$) = *obj\pCOMMON\name$
			EndIf
			If *obj\pCOMMON\font$ And IsValueSupported(*obj\pEXTRA\type$, #v_font) And Not (*obj\pCOMMON\font$ = GetDefaultValue(*obj\pExtra\type$, #v_font))
				If FindMapElement(XMLFonts(), *obj\pCOMMON\font$) = 0
					With XMLFonts(*obj\pCOMMON\font$)
						\Variable$ = "Font" + Str(MapSize(XMLFonts()) + 64)
						\FontName$ = StringField(*obj\pCOMMON\font$, 1, "|")
						\FontSize = Val(StringField(*obj\pCOMMON\font$, 2, "|"))
						\FontStyle = Val(StringField(*obj\pCOMMON\font$, 3, "|"))
					EndWith
				EndIf
				If *obj\pEXTRA\type$ = #C_DIALOGS
					AddElement(XMLCodeWindows(ParentWindow$)\Codes())
					With XMLCodeWindows(ParentWindow$)\Codes()
						\Code$ + "; Default UI font"
					EndWith
					AddElement(XMLCodeWindows(ParentWindow$)\Codes())
					With XMLCodeWindows(ParentWindow$)\Codes()
						\Code$ + "If IsFont(" + XMLFonts(*obj\pCOMMON\font$)\Variable$ + ") : SetGadgetFont(#PB_Default, FontID(" + XMLFonts(*obj\pCOMMON\font$)\Variable$ + ")) : EndIf"
					EndWith
				Else
					AddElement(XMLCodeWindows(ParentWindow$)\Codes())
					With XMLCodeWindows(ParentWindow$)\Codes()
						\Code$ + "If IsFont(" + XMLFonts(*obj\pCOMMON\font$)\Variable$ + ") : SetGadgetFont(" + *obj\pCOMMON\name$ + ", FontID(" + XMLFonts(*obj\pCOMMON\font$)\Variable$ + ")) : EndIf"
					EndWith
					XMLDeclaresWindows(ParentWindow$)\Controls$(*obj\pCOMMON\name$) = *obj\pCOMMON\name$
				EndIf
			EndIf
			If *obj\pCOMMON\variable$ And IsValueSupported(*obj\pEXTRA\type$, #v_variable) And Not (*obj\pCOMMON\variable$ = GetDefaultValue(*obj\pExtra\type$, #v_variable))
				XMLDeclaresWindows(ParentWindow$)\Controls$(*obj\pCOMMON\name$) = *obj\pCOMMON\name$
			EndIf
		EndIf
	EndProcedure

	; Translates XML_EXTRA_ATTRIBUTES and values to a node attributes
	; Attribute is only added if value is not empty
	; RETURN:		none
	Procedure pExtraStr (XmlNode, *obj.OBJECT)
		Select *obj\pEXTRA\Type$
			Case #C_GRIDBOX:
				AddAttribute(XmlNode, *obj,  #v_colexpand, 		*obj\pEXTRA\gridbox\colexpand$)
				AddAttribute(XmlNode, *obj,  #v_colspacing, 	*obj\pEXTRA\gridbox\colspacing$)
				AddAttribute(XmlNode, *obj,  #v_columns, 		*obj\pEXTRA\gridbox\columns$)
				AddAttribute(XmlNode, *obj,  #v_rowexpand, 		*obj\pEXTRA\gridbox\rowexpand$)
				AddAttribute(XmlNode, *obj,  #v_rowspacing, 	*obj\pEXTRA\gridbox\rowspacing$)
			Case #C_HBOX, #C_VBOX:
				AddAttribute(XmlNode, *obj,  #v_align, 			*obj\pEXTRA\hvbox\align$)
				AddAttribute(XmlNode, *obj,  #v_expand, 		*obj\pEXTRA\hvbox\expand$)
				AddAttribute(XmlNode, *obj,  #v_spacing, 		*obj\pEXTRA\hvbox\spacing$)
			Case #C_MULTIBOX:
			Case #C_SINGLEBOX
				AddAttribute(XmlNode, *obj,  #v_align, 			*obj\pEXTRA\singlebox\align$)
				AddAttribute(XmlNode, *obj,  #v_expand, 		*obj\pEXTRA\singlebox\expand$)
				AddAttribute(XmlNode, *obj,  #v_expandheight, 	*obj\pEXTRA\singlebox\expandheight$)
				AddAttribute(XmlNode, *obj,  #v_expandwidth, 	*obj\pEXTRA\singlebox\expandwidth$)
				AddAttribute(XmlNode, *obj,  #v_margin, 		*obj\pEXTRA\singlebox\margin$)
			Case #C_WINDOW:
				AddAttribute(XmlNode, *obj,  #v_minwidth, 		*obj\pEXTRA\window\minwidth$)
				AddAttribute(XmlNode, *obj,  #v_maxwidth, 		*obj\pEXTRA\window\maxwidth$)
				AddAttribute(XmlNode, *obj,  #v_minheight, 		*obj\pEXTRA\window\minheight$)
				AddAttribute(XmlNode, *obj,  #v_maxheight, 		*obj\pEXTRA\window\maxheight$)
			Case #C_OPTION:
				AddAttribute(XmlNode, *obj,  #v_group, 			*obj\pEXTRA\option\group$)
			Case #C_SCROLLAREA:
				AddAttribute(XmlNode, *obj,  #v_scrolling,		*obj\pEXTRA\scrollarea\scrolling$)
				AddAttribute(XmlNode, *obj,  #v_innerheight,	*obj\pEXTRA\scrollarea\innerheight$)
				AddAttribute(XmlNode, *obj,  #v_innerwidth,		*obj\pEXTRA\scrollarea\innerwidth$)
				AddAttribute(XmlNode, *obj,  #v_step,			*obj\pEXTRA\scrollarea\scrollstep$)
			Case #C_SCROLLBAR:
				AddAttribute(XmlNode, *obj,  #v_page,			*obj\pEXTRA\scrollbar\page$)
			Case #C_SPLITTER:
				AddAttribute(XmlNode, *obj,  #v_firstmin,		*obj\pEXTRA\splitter\firstmin$)
				AddAttribute(XmlNode, *obj,  #v_secondmin,		*obj\pEXTRA\splitter\secondmin$)
		EndSelect
	EndProcedure

	; Translates XML_EVENTS and values to a node attributes
	; Attribute is only added if value is not empty
	; ParentWindow$		name of window item to which current object belongs
	; RETURN:				none
	Procedure pEventStr (XmlNode, *obj.OBJECT, ParentWindow$)
		If Not XML_preview And *obj\pEVENTS\onevent$
			If LCase(*obj\pEVENTS\onevent$) = "yes"
				AddElement(XMLCallbackWindows(ParentWindow$)\Callbacks())
				With XMLCallbackWindows(ParentWindow$)\Callbacks()
					\Comment$ = LSet("[" + *obj\pEXTRA\Type$ + "]", 16, " ")
					\Name$ 		= "ecb_" + *obj\pCOMMON\name$ + "()"
					\obj 		= *obj
					\CBWindow$	= ParentWindow$
				EndWith
			Else
				AddElement(XMLCallbackWindows(ParentWindow$)\Callbacks())
				With XMLCallbackWindows(ParentWindow$)\Callbacks()
					\Comment$ = LSet("[array]", 16, " ")
					\Name$ 		= "ecb_" + *obj\pEVENTS\onevent$ + "()"
					\obj 		= *obj
					\CBWindow$	= ParentWindow$
				EndWith
			EndIf
			If XML_exportcode
				XMLDeclaresWindows(ParentWindow$)\Controls$(*obj\pCOMMON\name$) = *obj\pCOMMON\name$
			EndIf
			If XML_export
				SetXMLAttribute(XmlNode, #v_onevent_wrap, *obj\pEVENTS\onevent$)
			EndIf
		EndIf
	EndProcedure

	; Callback from the tree structure, used to compose XML tree
	; RETURN:		should be none
	Procedure GenerateXMLCB (*Item.TreeNode)
		Protected ParentIsGridbox
		Protected Parent = XML_root
		If *Item\Parent$
			If FindMapElement(XMLNodes(), *Item\Parent$)
				Parent = XMLNodes()
				ParentIsGridbox = Bool(Tree(*Item\Parent$)\Data\pEXTRA\Type$ = #C_GRIDBOX)
			EndIf
		EndIf
		Protected tNode
		Static ParentWindow$
		Select *Item\Data\pEXTRA\Type$
			Case #C_DIALOGS:
				ParentWindow$ = ""
				tNode = CreateXMLNode(Parent, *Item\Data\pEXTRA\Type$, -1)
				pCommonStr(tNode, *Item, #SPECIAL_KEY)
			Case #C_WINDOW:
				ParentWindow$ = *Item\Data\pCOMMON\name$
				If XML_preview And Not ParentWindow$ = XML_preview_window$
					ProcedureReturn
				EndIf
				tNode = CreateXMLNode(Parent, *Item\Data\pEXTRA\Type$, -1)
				If XML_preview
					Protected forPreview$ = *Item\Data\pCOMMON\flags$
					If FindString(*Item\Data\pCOMMON\flags$, "#PB_Window_Invisible", 1, #PB_String_NoCase) = 0
						If *Item\Data\pCOMMON\flags$
							*Item\Data\pCOMMON\flags$ + "|#PB_Window_Invisible"
						Else
							*Item\Data\pCOMMON\flags$ = "#PB_Window_Invisible"
						EndIf
					EndIf
				EndIf
				pCommonStr(tNode, *Item, "")
				pExtraStr(tNode, *Item)
				If XML_preview
					*Item\Data\pCOMMON\flags$ = forPreview$
				EndIf
			Default:
				If XML_preview And Not ParentWindow$ = XML_preview_window$
					ProcedureReturn
				EndIf
				tNode = CreateXMLNode(Parent, *Item\Data\pEXTRA\Type$, -1)
				pCommonStr(tNode, *Item, ParentWindow$, ParentIsGridbox)
				pExtraStr(tNode, *Item)
				pEventStr(tNode, *Item, ParentWindow$)
		EndSelect
		XMLNodes(*Item\Name$) = tNode
	EndProcedure

	; Generator main procedure
	; FixForPreview					if true, some changes are made to better fit resulting XML in preview
	; RawXMLExport					set it to true if function is called to save XML file
	; CodeExport					set to true if called to export code
	; PreviewWindowName$			a name of window item. is used if FixForPreview = true, else should be ""
	; RETURN:						true on success, 0 else
	;								XML_current variable contains XML tree and can be used from outside
	Procedure GenerateXML(FixForPreview, RawXMLExport, CodeExport, PreviewWindowName$)
		Protected speed.q = ElapsedMilliseconds()
		DPIx = 0
		DPIY = 0
		ClearMap(DPIv$())
		If IsXML(XML_current)
			FreeXML(XML_current)
		EndIf
		ClearMap(XMLCallbackWindows())
		ClearMap(XMLCodeWindows())
		ClearMap(XMLDeclaresWindows())
		ClearMap(XMLFonts())
		XML_preview = FixForPreview
		XML_export = RawXMLExport
		XML_exportcode = CodeExport
		XML_current = CreateXML(#PB_Any)
		If IsXML(XML_current)
			XML_root = RootXMLNode(XML_current)
			If XML_preview
				XML_preview_window$ = PreviewWindowName$
			Else
				XML_preview_window$ = ""
			EndIf
			TreeRecurse(Tree(), #C_DIALOGS, @GenerateXMLCB())
			If XML_preview = #False
				FormatXML(XML_current, #PB_XML_WindowsNewline | #PB_XML_ReduceNewline| #PB_XML_ReIndent | #PB_XML_ReduceSpace | #PB_XML_ReFormat, 2)
			EndIf
		EndIf
		ClearMap(XMLNodes())
		Debug "[delay] GenerateXML: " + Str(ElapsedMilliseconds() - speed)
		ProcedureReturn IsXML(XML_current)
	EndProcedure

	; Launches generation and saves resulting XML to a text file
	; RETURN:			non-zero on success
	Procedure SaveXMLFile(UseClipboard)
		If TreeLogicCanExport() = #False
			ProcedureReturn 0
		EndIf
		If GenerateXML(#False, #True, #False, "") = #False
			ProcedureReturn 0
		EndIf
		Protected tXML$
		If UseClipboard
			ClearClipboard()
			If IsXML(XML_current)
				tXML$ = ComposeXML(XML_current)
				Delay(50)
				SetClipboardText(tXML$)
				XMLSetState(XMLHash())
			EndIf
			ProcedureReturn #True
		EndIf
		Protected XmlPath$
		Protected Res
		If cfg\IOLastDIR$
			XmlPath$ = cfg\IOLastDIR$
		Else
			XmlPath$ = ""
		EndIf
		XmlPath$ + "UI_" + FormatDate("%yyyy-%mm-%dd_%hh-%ii-%ss", Date()) + ".xml"
		XmlPath$ = SaveFileRequester("Export XML", XmlPath$, "XML file (*.xml)|*.xml|All files (*.*)|*.*", 0)
		If XmlPath$
			If SelectedFilePattern() = 0 And Not LCase(GetExtensionPart(XmlPath$)) = "xml"
				XmlPath$ + ".xml"
			EndIf
			cfg\IOLastDIR$ = GetPathPart(XmlPath$)
			If IsXML(XML_current)
				Res = SaveXML(XML_current, XmlPath$)
			EndIf
			If Res = #False
				MessageRequester("Export XML ERROR", "Can't save XML to " + XmlPath$, #PB_MessageRequester_Error)
			Else
				WndTreeSetCaption(": " + GetFilePart(XmlPath$))
				XMLSetState(XMLHash())
			EndIf
		EndIf
		ProcedureReturn Res
	EndProcedure

	; Launches generation and saves result as a code file
	; RETURN:			non-zero on success
	Procedure SaveCodeFile(UseClipboard)
		If TreeLogicCanExport() = #False
			ProcedureReturn
		EndIf
		Macro TAB(n)
			RSet(#Null$, n, #TAB$)
		EndMacro

		If Not UseClipboard
			Protected XmlPath$
			If cfg\IOLastDIR$
				XmlPath$ = cfg\IOLastDIR$
			Else
				XmlPath$ = ""
			EndIf
			XmlPath$ + "UI_" + FormatDate("%yyyy-%mm-%dd_%hh-%ii-%ss", Date()) + ".pb"
			XmlPath$ = SaveFileRequester("Export this hardcode..", XmlPath$, "PB source file (*.pb)|*.pb|All files (*.*)|*.*", 0)
			If XmlPath$
				If SelectedFilePattern() = 0 And Not LCase(GetExtensionPart(XmlPath$)) = "pb"
					XmlPath$ + ".pb"
				EndIf
				cfg\IOLastDIR$ = GetPathPart(XmlPath$)
			Else
				ProcedureReturn 0
			EndIf
		EndIf
		Protected.q speed = ElapsedMilliseconds()
		If GenerateXML(#False, #False, #True, "") = #False
			ProcedureReturn 0
		EndIf
		Protected Res
		If IsXML(XML_current)
			Protected NewList ExportedWindows$()
			If TreeGetChildCount(Tree(), #C_DIALOGS) >= 1
				ForEach Tree(#C_DIALOGS)\Childs$()
					AddElement(ExportedWindows$())
					ExportedWindows$() = Tree(Tree(#C_DIALOGS)\Childs$())\Data\pCOMMON\name$
				Next
			EndIf
			SortList(ExportedWindows$(), #PB_Sort_Ascending)
			;{ Generate 'Dialogs XML' section }

				Protected XMLDpi$ = ""
				If cfg\DPIPatch And MapSize(DPIv$()) >= 1
					XMLDpi$ = ";{ DPI Patch (Windows) }" + #CRLF$ + #CRLF$
					XMLDpi$ + TAB(1) + "; Don't forget related manifest file or SetDPIAware() API call!" + #CRLF$ + #CRLF$
					XMLDpi$ + TAB(1) + "Global.f DPIXScale = 1.0, DPIYScale = 1.0" + #CRLF$
					XMLDpi$ + TAB(1) + "Procedure InitScaleDPI()" + #CRLF$
					XMLDpi$ + TAB(2) + "Protected lpx, lpy, DC = GetDC_(#Null)" + #CRLF$
					XMLDpi$ + TAB(2) + "If DC" + #CRLF$
					XMLDpi$ + TAB(3) +"lpx = GetDeviceCaps_(DC, #LOGPIXELSX) : lpy = GetDeviceCaps_(DC, #LOGPIXELSY)" + #CRLF$
					XMLDpi$ + TAB(3) +"If lpx : DPIXScale = lpx / 96.0 : EndIf" + #CRLF$
					XMLDpi$ + TAB(3) +"If lpy : DPIYScale = lpy / 96.0 : EndIf" + #CRLF$
					XMLDpi$ + TAB(3) +"ReleaseDC_(#Null, DC)" + #CRLF$
					XMLDpi$ + TAB(2) + "EndIf" + #CRLF$
					XMLDpi$ + TAB(1) + "EndProcedure" + #CRLF$
					XMLDpi$ + TAB(1) + "InitScaleDPI()" + #CRLF$
					XMLDpi$ + TAB(1) + "Macro ScaleX (x) : 	((x) * DPIXScale) :	EndMacro" + #CRLF$
					XMLDpi$ + TAB(1) + "Macro ScaleY (y) :	((y) * DPIYScale) :	EndMacro" + #CRLF$
					XMLDpi$ + #CRLF$
					ForEach DPIv$()
						XMLDpi$ + TAB(1) + "Define " + MapKey(DPIv$()) + " = " + DPIv$() + " : Runtime " + MapKey(DPIv$()) + #CRLF$
					Next
					XMLDpi$ + #CRLF$
					XMLDpi$ + ";}" + #CRLF$ + #CRLF$
				EndIf
				Protected Dim rXML$(0)
				Protected Dim fXML$(0)
				Protected t, c, n, nstart
				Protected LitDivider$ = "DialogsXML$ +"
				Protected tXML$ = ComposeXML(XML_current, #PB_XML_NoDeclaration)
				tXML$ = ReplaceString(tXML$, #DOUBLEQUOTE$, "'")
				t = SplitS(rXML$(), tXML$, #CRLF$)
				ReDim fXML$(t + 128)		; 128 is reserved for "DialogsXML$ +" separators
				fXML$(nstart) = "Global DialogsXML$ = #Empty$ +"
				nstart+1
				n = nstart-1
				Protected fLen, AllLen
				For c = 1 To t
					If Not Trim(rXML$(c)) = ""
						n + 1
						fXML$(n) = rXML$(c)
						AllLen + Len(fXML$(n))
						If AllLen >= 7372 ; leave 10% of free literal size for case if user wants to edit dialog manually
							AllLen = 0
							n + 1
							fXML$(n) = LitDivider$
						EndIf
					EndIf
				Next c
				FreeArray(rXML$())
				Protected Spaces, Spaces$
				t = 0
				For c = nstart To n
					fLen = Len(fXML$(c))
					fXML$(c) = LTrim(fXML$(c))
					Spaces = fLen - Len(fXML$(c))
					Spaces / 2
					Spaces$ = TAB(Spaces)
					fXML$(c) = TAB(1) + #DOUBLEQUOTE$ + Spaces$ + fXML$(c) + #DOUBLEQUOTE$
					If fXML$(c + 1) = LitDivider$
						c + 1
					ElseIf Not c = n
						fXML$(c) + " +"
					EndIf
				Next c
				ReDim fXML$(n)
				tXML$ = JoinS(fXML$(), #CRLF$)
				FreeArray(fXML$())
			;}

			;{ Generate code }

				Protected TCBDeclares$
				Protected NewList TCBControls$()
				If ListSize(ExportedWindows$()) >= 1
					TCBDeclares$ + #CRLF$
					ForEach ExportedWindows$()
						TCBDeclares$ + TAB(1) + "Global dlg_" + ExportedWindows$() + " = CreateDialog(#PB_Any)" + #CRLF$
						TCBDeclares$ + TAB(1) + "Global " + ExportedWindows$() + #CRLF$
						ClearList(TCBControls$())
						If FindMapElement(XMLDeclaresWindows(), ExportedWindows$())
							ForEach XMLDeclaresWindows()\Controls$()
								AddElement(TCBControls$())
								TCBControls$() = XMLDeclaresWindows()\Controls$()
							Next
						EndIf
						If ListSize(TCBControls$()) >= 1
							SortList(TCBControls$(), #PB_Sort_Ascending|#PB_Sort_NoCase)
							FirstElement(TCBControls$())
							TCBDeclares$ + TAB(1) + "Global "
							Repeat
								TCBDeclares$ + TCBControls$()
								If NextElement(TCBControls$())
									TCBDeclares$ +  ", "
								Else
									TCBDeclares$ +  #CRLF$
									Break
								EndIf
							ForEver
						EndIf
						TCBDeclares$ + #CRLF$
					Next
				EndIf
				Protected NewList XMLFontsSorted.ITEMFONTINFO()
				If MapSize(XMLFonts()) >= 1
					ForEach XMLFonts()
						AddElement(XMLFontsSorted())
						XMLFontsSorted() = XMLFonts()
					Next
					SortStructuredList(XMLFontsSorted(), #PB_Sort_Ascending, OffsetOf(ITEMFONTINFO\Variable$), #PB_String)
					Protected FontStyles$
					ForEach XMLFontsSorted()
						FontStyles$ = ""
						If XMLFontsSorted()\FontStyle & #PB_Font_Bold
							FontStyles$ = "#PB_Font_Bold"
						EndIf
						If XMLFontsSorted()\FontStyle & #PB_Font_Italic
							If FontStyles$
								FontStyles$ + " | "
							EndIf
							FontStyles$ + "#PB_Font_Italic"
						EndIf
						TCBDeclares$ + TAB(1) + "Global " + XMLFontsSorted()\Variable$ + " = LoadFont(#PB_Any, " + #DOUBLEQUOTE$ + XMLFontsSorted()\FontName$ + #DOUBLEQUOTE$	+ ", " + Str(XMLFontsSorted()\FontSize)
						If FontStyles$
							TCBDeclares$ + ", " + FontStyles$ + ")" + #CRLF$
						Else
							TCBDeclares$ + ")" + #CRLF$
						EndIf
					Next
					TCBDeclares$ + #CRLF$
				EndIf
				Protected TCBCode$
				Protected NewList CEvents$()
				Structure CINST
					Name$
					Comment$
					List *ObjList.OBJECT()
				EndStructure
				Protected NewMap CInstances.CINST()
				Protected CArray
				ForEach ExportedWindows$()
					TCBCode$ + TAB(1) + ";{ Callbacks: " + ExportedWindows$() + " }" + #CRLF$
					TCBCode$ + #CRLF$
					If FindMapElement(XMLCallbackWindows(), ExportedWindows$())
						SortStructuredList(XMLCallbackWindows()\Callbacks(), #PB_Sort_Ascending, OffsetOf(ITEMCALLBACK\Name$), #PB_String)
						ClearMap(CInstances())
						ForEach XMLCallbackWindows()\Callbacks()
							With XMLCallbackWindows()\Callbacks()
								If FindMapElement(CInstances(), LCase(\Name$)) = 0
									CInstances(LCase(\Name$))\Comment$ = \Comment$
									CInstances(LCase(\Name$))\Name$ = \Name$
									AddElement(CInstances(LCase(\Name$))\ObjList())
									CInstances(LCase(\Name$))\ObjList() = \obj
								Else
									AddElement(CInstances(LCase(\Name$))\ObjList())
									CInstances(LCase(\Name$))\ObjList() = \obj
								EndIf
							EndWith
						Next
						ForEach CInstances()
							If ListSize(CInstances()\ObjList()) = 1
								CArray = #False
							Else
								CArray = #True
							EndIf
							With CInstances()
								TCBCode$ + TAB(2) + "; " + \Comment$ + #CRLF$
								TCBCode$ + TAB(2) + "Procedure " + \Name$ + #CRLF$
								If CArray
									TCBCode$ + TAB(3) + "Select EventGadget()" + #CRLF$
								EndIf
								ForEach CInstances()\ObjList()
									GetSupportedEventsList(\ObjList()\pEXTRA\type$, CEvents$())
									If CArray
										TCBCode$ + TAB(4) + "Case " + \ObjList()\pCOMMON\name$ + ":" + #CRLF$
									EndIf
									If ListSize(CEvents$()) >= 1
										TCBCode$ + TAB(3 + CArray*2) + "Select EventType()" + #CRLF$
										ForEach CEvents$()
											TCBCode$ + TAB(4 + CArray*2) + "Case " + CEvents$() + ": " + #CRLF$
											TCBCode$ + TAB(5 + CArray*2) + "Debug " + #DOUBLEQUOTE$ + \ObjList()\pCOMMON\name$ + ": " + RemoveString(CEvents$(), "#PB_EventType_", #PB_String_NoCase) + #DOUBLEQUOTE$ + #CRLF$
										Next
										TCBCode$ + TAB(4 + CArray*2) + "Default: " + #CRLF$
										TCBCode$ + TAB(5 + CArray*2) + "Debug " + #DOUBLEQUOTE$ + \ObjList()\pCOMMON\name$ + ": " + "Event " + #DOUBLEQUOTE$ + " + EventType()" + #CRLF$
										TCBCode$ + TAB(3 + CArray*2) + "EndSelect" + #CRLF$
									Else
										TCBCode$ + TAB(3 + CArray*2) + "Debug " + #DOUBLEQUOTE$ + \ObjList()\pCOMMON\name$ + ": " + "Event " + #DOUBLEQUOTE$ + " + EventType()" + #CRLF$
									EndIf
								Next
								If CArray
									TCBCode$ + TAB(3) + "EndSelect" + #CRLF$
								EndIf
								TCBCode$ + TAB(2) + "EndProcedure" + #CRLF$
								TCBCode$ + #CRLF$
							EndWith
						Next
					Else
						TCBCode$ + TAB(2) + "; Event handlers here" + #CRLF$
						TCBCode$ + #CRLF$
					EndIf
					TCBCode$ + TAB(1) + ";}" + #CRLF$ + #CRLF$
				Next
				TCBCode$ + #CRLF$
				TCBCode$ + TAB(1) + "Procedure InitDialogs()" + #CRLF$
				TCBCode$ + TAB(2) + "Protected DialogsXML = ParseXML(#PB_Any, DialogsXML$)" + #CRLF$
				If FindMapElement(XMLCodeWindows(), #SPECIAL_KEY)
					ForEach XMLCodeWindows()\Codes()
						With XMLCodeWindows()\Codes()
							TCBCode$ + TAB(2) + \Code$ + #CRLF$
						EndWith
					Next
				EndIf
				If ListSize(ExportedWindows$()) >= 1
					TCBCode$ + #CRLF$
					TCBCode$ + TAB(2) + "; Open dialogs, enum controls, bind events, customize" + #CRLF$
					ForEach ExportedWindows$()
						TCBCode$ + TAB(2) + "If OpenXMLDialog(" + "dlg_" + ExportedWindows$() + ", DialogsXML, " + #DOUBLEQUOTE$ + ExportedWindows$() + #DOUBLEQUOTE$ + ")" + #CRLF$
						TCBCode$ + TAB(3) + ExportedWindows$() + " = DialogWindow(dlg_" + ExportedWindows$() + ")" + #CRLF$
						ClearList(TCBControls$())
						If FindMapElement(XMLDeclaresWindows(), ExportedWindows$())
							ForEach XMLDeclaresWindows()\Controls$()
								AddElement(TCBControls$())
								TCBControls$() = TAB(3) + XMLDeclaresWindows()\Controls$() + " = DialogGadget(dlg_" + ExportedWindows$() + ~", \"" + XMLDeclaresWindows()\Controls$() + ~"\")"
							Next
						EndIf
						If FindMapElement(XMLCallbackWindows(), ExportedWindows$())
							ForEach XMLCallbackWindows()\Callbacks()
								With XMLCallbackWindows()\Callbacks()
									If FindMapElement(XMLDeclaresWindows(), ExportedWindows$()) And FindMapElement(XMLDeclaresWindows()\Controls$(), \obj\pCOMMON\name$)
									Else
										AddElement(TCBControls$())
										TCBControls$() = TAB(3) + \obj\pCOMMON\name$ + " = DialogGadget(dlg_" + ExportedWindows$() + ~", \"" + \obj\pCOMMON\name$ + ~"\")"
									EndIf
								EndWith
							Next
						EndIf
						If ListSize(TCBControls$()) >= 1
							SortList(TCBControls$(), #PB_Sort_Ascending|#PB_Sort_NoCase)
							FirstElement(TCBControls$())
							TCBCode$ + #CRLF$
							Repeat
								TCBCode$ + TCBControls$()
								If NextElement(TCBControls$())
									TCBCode$ +  #CRLF$
								Else
									TCBCode$ +  #CRLF$
									Break
								EndIf
							ForEver
						EndIf
						If FindMapElement(XMLCallbackWindows(), ExportedWindows$())
							TCBCode$ + #CRLF$
							ClearList(TCBControls$())
							ForEach XMLCallbackWindows()\Callbacks()
								With XMLCallbackWindows()\Callbacks()
									AddElement(TCBControls$())
									TCBControls$() = TAB(3) + "BindGadgetEvent(" + \obj\pCOMMON\name$ + ", @" + \Name$ + ")"
								EndWith
							Next
							If ListSize(TCBControls$()) >= 1
								SortList(TCBControls$(), #PB_Sort_Ascending|#PB_Sort_NoCase)
								FirstElement(TCBControls$())
								Repeat
									TCBCode$ + TCBControls$()
									If NextElement(TCBControls$())
										TCBCode$ + #CRLF$
									Else
										TCBCode$ + #CRLF$
										Break
									EndIf
								ForEver
							EndIf
						EndIf
						If FindMapElement(XMLCodeWindows(), ExportedWindows$())
							TCBCode$ + #CRLF$
							ForEach XMLCodeWindows()\Codes()
								With XMLCodeWindows()\Codes()
									TCBCode$ + TAB(3) + \Code$ + #CRLF$
								EndWith
							Next
						EndIf
						TCBCode$ + #CRLF$
						TCBCode$ + TAB(3) + "RefreshDialog(" + "dlg_" + ExportedWindows$() + ")" + #CRLF$
						TCBCode$ + TAB(2) + "EndIf" + #CRLF$
					Next
					TCBCode$ + #CRLF$
					TCBCode$ + TAB(2) + "; Cls" + #CRLF$
					TCBCode$ + TAB(2) + "If IsXML(DialogsXML): FreeXML(DialogsXML): EndIf" + #CRLF$
					TCBCode$ + TAB(2) + "DialogsXML$ = #Empty$" + #CRLF$
				EndIf
				TCBCode$ + TAB(1) + "EndProcedure" + #CRLF$
			;}

			;{ Compose }

				Protected Result$ = "EnableExplicit" + #CRLF$  + #CRLF$
				Result$ + XMLDpi$
				Result$ + ";{ Dialogs XML }" + #CRLF$ + #CRLF$
				Result$ + "; Generated with " + APP_TITLE$ + FormatDate(", %yyyy/%mm/%dd %hh:%ii:%ss", Date()) + #CRLF$ +
							tXML$ + #CRLF$ + #CRLF$ + ";}" + #CRLF$ + #CRLF$
				Result$ + ";{ Dialogs UI }" + #CRLF$ + TCBDeclares$ + #CRLF$ + TCBCode$ + #CRLF$ + ";}" + #CRLF$ + #CRLF$
				If cfg\NoLoop = #False
					Result$ + #CRLF$ + "; Main" + #CRLF$ + "InitDialogs()" + #CRLF$ + #CRLF$
					Result$ + "Repeat" + #CRLF$
					Result$ + TAB(1) + "Define E = WaitWindowEvent()" + #CRLF$
					Result$ + TAB(1) + "Select E" + #CRLF$
					Result$ + TAB(2) + "Case #PB_Event_CloseWindow: break" + #CRLF$
					Result$ + TAB(1) + "EndSelect" + #CRLF$
					Result$ + "Forever" + #CRLF$
				EndIf
			;}

			If UseClipboard
				ClearClipboard()
				SetClipboardText(Result$)
				Res = #True
			Else
				Protected hFile = CreateFile(#PB_Any, XmlPath$, #PB_UTF8 | #PB_File_NoBuffering)
				If IsFile(hFile)
					WriteStringFormat(hFile, #PB_UTF8)
					WriteString(hFile, Result$, #PB_UTF8)
					CloseFile(hFile)
					Res = #True
					If cfg\RunCode
						RunProgram(XmlPath$)
					EndIf
				EndIf
			EndIf
		EndIf
		If Res = #False
			If UseClipboard
				MessageRequester("Export code ERROR", "Can't export to clipboard (wtf??)", #PB_MessageRequester_Error)
			Else
				MessageRequester("Export code ERROR", "Can't save results to " + XmlPath$, #PB_MessageRequester_Error)
			EndIf
		EndIf
		Debug "[delay] SaveCodeFile: " + Str(ElapsedMilliseconds() - speed)
		UndefineMacro TAB
		ProcedureReturn Res
	EndProcedure

	; Stores current tree hash for future checks
	Procedure XMLSetState(hash$)
		XML_lastHash$ = hash$
	EndProcedure

	; Get stored tree hash
	Procedure$ XMLGetState()
		ProcedureReturn XML_lastHash$
	EndProcedure
	; Calculate current tree hash
	Procedure$ XMLHash()
		Protected TH$
		UseCRC32Fingerprint()
		If GenerateXML(#False, #True, #False, "")
			TH$ = StringFingerprint(ComposeXML(XML_current), #PB_Cipher_CRC32)
		EndIf
		ProcedureReturn TH$
	EndProcedure

;}

;{ XML		: load file }

	CompilerIf Defined(IS_FULL, #PB_Constant) ; this is full version (load XML routines)
		Global LoadXMLErrors$
		Global NewMap LoadXMLCache$()
		; Process attributes of given node and transfer them to given object
		; RETURN:		true on success, false if object type is unknown
		Procedure ReadNodeAttributes(*out.OBJECT, ObjType$, node)
			Protected res
			If Not isValidObject(ObjType$)
				LoadXMLErrors$ + "Unsupported object type: " + ObjType$ + #CRLF$
				res = #False
			Else
				ClearStructure(*out, OBJECT)
				*out\pEXTRA\type$ = ObjType$
				Protected PropName$, PropValue$
				If ExamineXMLAttributes(Node)
					Repeat
						If NextXMLAttribute(Node)
							PropName$ = LCase(XMLAttributeName(Node))
							PropValue$ = XMLAttributeValue(Node)
							Select PropName$
								Case #v_forecolor_wrap:
									PropName$ = #v_forecolor
								Case #v_backcolor_wrap:
									PropName$ = #v_backcolor
								Case #v_font_wrap:
									PropName$ = #v_font
								Case #v_tooltip_wrap:
									PropName$ = #v_tooltip
								Case #v_variable_wrap:
									PropName$ = #v_variable
								Case #v_onevent_wrap:
									PropName$ = #v_onevent
							EndSelect
							If IsValueSupported(ObjType$, PropName$)
								SetValue(*out, PropName$, PropValue$)
							Else
								LoadXMLErrors$ + "Unsupported property: " + ObjType$ + "\" + PropName$ + #CRLF$
							EndIf
						Else
							Break
						EndIf
					ForEver
				EndIf
				res = #True
			EndIf
			ProcedureReturn res
		EndProcedure

		Procedure LoadXMLRecurse (Node)
			If XMLNodeType(Node) = #PB_XML_Normal
				Protected NChilds
				Protected ObjType$ = LCase(GetXMLNodeName (Node))
				Protected NEW.OBJECT
				If XMLNodeType(ParentXMLNode(Node)) = #PB_XML_Normal
					If ReadNodeAttributes (NEW, ObjType$, Node)
						If objNew(LoadXMLCache$(Str(ParentXMLNode(Node))), NEW\pEXTRA\type$, NEW\pCOMMON\name$, NEW, #PB_List_Last, #False)
							LoadXMLCache$(Str(Node)) = LCase(NEW\pCOMMON\name$)
							NChilds = XMLChildCount(Node)
						Else
							LoadXMLErrors$ +  "Cannot add object: " + NEW\pCOMMON\name$ + #CRLF$
						EndIf
					EndIf
				Else
					ReadNodeAttributes (NEW, ObjType$, Node)
					If ObjType$ = #C_DIALOGS And objNew("", #C_DIALOGS, #C_DIALOGS, NEW)
						LoadXMLCache$(Str(Node)) = #C_DIALOGS
						NChilds = XMLChildCount(Node)
					Else
						LoadXMLErrors$ + "Cannot add main object: " + ObjType$ + #CRLF$
						ProcedureReturn
					EndIf
				EndIf
				If NChilds
					LoadXMLRecurse (ChildXMLNode(Node, 1))
				EndIf
			EndIf
			Protected NNode = NextXMLNode (Node)
			If NNode
				LoadXMLRecurse(NNode)
			EndIf
		EndProcedure

		; Load given XML file into Tree()
		Procedure LoadXMLFile (XmlPath$)
			If FileSize(XmlPath$) < 0
				ProcedureReturn 0
			EndIf
			cfg\IOLastDIR$ = GetPathPart(XmlPath$)
			If cfg\ConfirmChanges And Not (XMLHash() = XMLGetState())
				If MessageRequester("Load XML", ~"There are unsaved changes in current XML. Continue import?", #PB_MessageRequester_Info | #PB_MessageRequester_YesNo) = #PB_MessageRequester_No
					ProcedureReturn
				EndIf
			EndIf
			Protected tXML = LoadXML(#PB_Any, XmlPath$)
			Protected T
			Protected Init = #False
			If IsXML(tXML)
				If XMLStatus(tXML) = #PB_XML_Success
					T = MainXMLNode(tXML)
					If T And XMLNodeType(T) = #PB_XML_Normal And GetXMLNodeName(T) = #C_DIALOGS
						ClearMap(Tree())
						Init = #True
						LoadXMLRecurse(T)
					EndIf
				Else
					LoadXMLErrors$ = "XML line " + Str(XMLErrorLine(tXML)) + ", pos " + Str(XMLErrorPosition(tXML)) + ": " + XMLError(tXML)
				EndIf
				ClearMap(LoadXMLCache$())
				FreeXML(tXML)
			Else
				LoadXMLErrors$ = "Not a valid XML file"
			EndIf
			If LoadXMLErrors$
				MessageRequester("Import XML", "Following error(s) occured with file '" + GetFilePart(XmlPath$) + "':" + #CRLF$ + #CRLF$ + LoadXMLErrors$, #PB_MessageRequester_Warning)
				LoadXMLErrors$ = ""
			EndIf
			If Init
				If FindMapElement(Tree(), #C_DIALOGS) = 0 Or Tree(#C_DIALOGS)\Parent$ <> ""
					MessageRequester("Import XML file", "Error: missing or invalid root item in loaded file", #PB_MessageRequester_Error)
					ClearMap(Tree())
					objNew("", #C_DIALOGS, #C_DIALOGS)
					WndTreeSetCaption("")
				Else
					WndTreeSetCaption(": " + GetFilePart(XmlPath$))
				EndIf
				Protected NewMap Expanded()
				Expanded(#C_DIALOGS) = #True
				RedrawTree(Expanded(), #C_DIALOGS)
				RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
				WndParamsList(TreeSelectedObject())
				WndViewSet(TreeSelectedWindow())
			EndIf
			XMLSetState(XMLHash())
		EndProcedure

		; Shows dialog to select and load DD2 file
		Procedure OpenXMLFile()
			Protected DD2Path$
			If cfg\IOLastDIR$
				DD2Path$ = cfg\IOLastDIR$
			Else
				DD2Path$ = ""
			EndIf
			DD2Path$ = OpenFileRequester("Import XML", DD2Path$, "XML file (*.xml)|*.xml|All files (*.*)|*.*", 0)
			If DD2Path$
				ProcedureReturn LoadXMLFile(DD2Path$)
			Else
				ProcedureReturn 0
			EndIf
		EndProcedure

		; Load file from drag&drop
		Procedure DropXMLFile()
			Protected InFile$ = StringField(EventDropFiles(), 1, #LF$)
			ProcedureReturn LoadXMLFile(InFile$)
		EndProcedure

	CompilerEndIf
;}

;{ UI 	: treeview wnd }

	Global TREE_WND_OFFSETX = ScaleX(8)		; free space around param window
	Global TREE_WND_OFFSETY = ScaleY(8)		;
	Global TREE_WND_WIDTH = ScaleX(240)		; param window width
	Global TREE_WND_HEIGHT = ScaleY(480)	; param window width
	Global TreeViewFont = LoadFont(#PB_Any, "Tahoma", 9)
	Global lpPrevWndFunc
	; TreeView control msg handler
	; Currently it only handles keyboard shortcut
	Procedure Tree_listMsg(hWnd, Msg, wParam, lParam)
	  Select Msg
		Case #WM_KEYUP:
		  Select wParam
		  	Case #VK_DELETE:
		  		TreeDeleteNode(#False)
		  	Case #VK_F2:
		  		TreeEditInit()
		  	Case #VK_X:
		  		If GetAsyncKeyState_(#VK_LCONTROL) Or GetAsyncKeyState_(#VK_RCONTROL)
					TreeCutNode()
		  		EndIf
		  	Case #VK_V:
		  		If GetAsyncKeyState_(#VK_LCONTROL) Or GetAsyncKeyState_(#VK_RCONTROL)
		  			TreePasteNode()
		  		EndIf
		  	Case #VK_C:
		  		If GetAsyncKeyState_(#VK_LCONTROL) Or GetAsyncKeyState_(#VK_RCONTROL)
		  			TreeCopyNode()
		  		EndIf
		  	Case #VK_E:
		  		If GetAsyncKeyState_(#VK_LCONTROL) Or GetAsyncKeyState_(#VK_RCONTROL)
		  			SaveCodeFile(cfg\Export2Clipboard)
		  		EndIf
		  	Case #VK_S:
		  		If GetAsyncKeyState_(#VK_LCONTROL) Or GetAsyncKeyState_(#VK_RCONTROL)
		  			SaveXMLFile(cfg\Export2Clipboard)
		  		EndIf
		  	Case #VK_O:
		  		CompilerIf Defined(IS_FULL, #PB_Constant) ; this is full version
		  		If GetAsyncKeyState_(#VK_LCONTROL) Or GetAsyncKeyState_(#VK_RCONTROL)
						OpenXMLFile()
		  		EndIf
		  		CompilerEndIf
		  EndSelect
	  EndSelect
	  ProcedureReturn CallWindowProc_(lpPrevWndFunc, hWnd, Msg, wParam, lParam)
	EndProcedure

	; Generic msg handler for Tree window
	Procedure WndTreeMsg(Window, Message, wParam, lParam)
		Protected result = #PB_ProcessPureBasicEvents
		Protected Tree_hwnd, TreeParent_hwnd
		Select Message
			Case #WM_NOTIFY: ; generic MSG
				Protected *pvdi.NMTVDISPINFO = lParam
				Select *pvdi\hdr\code
					Case #TVN_BEGINLABELEDIT:
						result = TreeEditStart (*pvdi)
					Case #TVN_ENDLABELEDIT:
						result = TreeEditEnd(*pvdi)
					Case #TVN_BEGINDRAG:
						result = TreeDragStart(lParam)
				EndSelect
			Case #WM_MOUSEMOVE: ; tree drag in progress
				result = TreeDrag(LParam)
			Case #WM_LBUTTONUP: ; tree drag finish
				result = TreeDragEnd()
		EndSelect
		ProcedureReturn result
	EndProcedure

	; Updates tree window title
	; RETURN:		none
	Procedure WndTreeSetCaption(Text$)
		SetWindowTitle(WndTree, "Interface Tree" + Text$)
	EndProcedure

	Global WNDTREE_SIZETMR = 'tr'
	Procedure WndTreeSizeTmr()
		Static lW, lH
		Protected cW = WindowWidth(WndTree)
		Protected cH = WindowHeight(WndTree)
		RemoveWindowTimer(WndTree, WNDTREE_SIZETMR)
		If Not cw = lw Or Not ch = lh
			If lw And lh
				ResizeGadget(WndTree_list, TREE_WND_OFFSETX, TREE_WND_OFFSETY, WindowWidth(WndTree) - TREE_WND_OFFSETX*2, WindowHeight(WndTree) - TREE_WND_OFFSETY*2)
			EndIf
			lw = cW
			lh = cH
		EndIf
	EndProcedure

	;{ Tree Icons }

		; "wasted time, my wooden crates are much better" <c>
		; Procedure returns ImageID of related control image
		; objType$			type of object
		; RETURN:			ImageID or 0 if fail
		Procedure TreeCargoIco(objType$)
			Static hIco, hRIco, hWIco, hCIco
			If hIco = 0
				Protected LstBackColor = GetGadgetColor(WndTree_list, #PB_Gadget_BackColor)
				hRIco = CreateImage(#PB_Any, 16, 16, 24, LstBackColor)
				hWIco = CreateImage(#PB_Any, 16, 16, 24, LstBackColor)
				hCIco = CreateImage(#PB_Any, 16, 16, 24, LstBackColor)
				hIco = CreateImage(#PB_Any, 16, 16, 24, LstBackColor)
				Protected tX, tY
				Protected r0 = 180, r1 = r0 * 0.75
				If StartDrawing(ImageOutput(hRIco))
					DrawingMode(#PB_2DDrawing_Gradient)
 					GradientColor(1.0, LstBackColor)
 					GradientColor(0.8, LstBackColor)
 					GradientColor(0.7, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
 					GradientColor(0.0, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
					BoxedGradient(0, 0, OutputWidth()-1, OutputHeight()-1)
					Box(0, 0, OutputWidth(), OutputHeight())
					StopDrawing()
				EndIf
				r0 = r0 * 1.1 : r1 = r0 * 0.75
				If StartDrawing(ImageOutput(hWIco))
					DrawingMode(#PB_2DDrawing_Gradient)
 					GradientColor(1.0, LstBackColor)
 					GradientColor(0.7, LstBackColor)
 					GradientColor(0.6, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
 					GradientColor(0.0, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
					BoxedGradient(0, 0, OutputWidth()-1, OutputHeight()-1)
					Box(0, 0, OutputWidth(), OutputHeight())
					StopDrawing()
				EndIf
				r0 = r0 * 1.1 : r1 = r0 * 0.75
				If StartDrawing(ImageOutput(hCIco))
					DrawingMode(#PB_2DDrawing_Gradient)
 					GradientColor(1.0, LstBackColor)
 					GradientColor(0.6, LstBackColor)
 					GradientColor(0.5, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
 					GradientColor(0.0, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
					BoxedGradient(0,0, OutputWidth()-1, OutputHeight()-1)
					Box(0, 0, OutputWidth(), OutputHeight())
					StopDrawing()
				EndIf
				r0 = r0 * 1.1 : r1 = r0 * 0.75
				If StartDrawing(ImageOutput(hIco))
					DrawingMode(#PB_2DDrawing_Gradient)
 					GradientColor(1.0, LstBackColor)
 					GradientColor(0.5, LstBackColor)
 					GradientColor(0.4, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
 					GradientColor(0.0, RGB(Random(r0, r1), Random(r0, r1), Random(r0, r1)))
					BoxedGradient(0,0, OutputWidth()-1, OutputHeight()-1)
					Box(0, 0, OutputWidth(), OutputHeight())
					StopDrawing()
				EndIf
			EndIf
			If objType$ = #C_DIALOGS
				ProcedureReturn ImageID(hRIco)
			ElseIf objType$ = #C_WINDOW
				ProcedureReturn ImageID(hWIco)
			ElseIf IsContainer(objType$)
				ProcedureReturn ImageID(hCIco)
			Else
				ProcedureReturn ImageID(hIco)
			EndIf
		EndProcedure

	;}

	;{ Generic tree window functions }

		Procedure WndTreeSize()
			AddWindowTimer(WndTree, WNDTREE_SIZETMR, 24)
		EndProcedure

		Procedure WndTreeClose()
		EndProcedure
		Procedure WndTreeOpen()
			WndTree = OpenWindow(#PB_Any, 0, 0, TREE_WND_WIDTH, TREE_WND_HEIGHT, "Interface Tree", #PB_Window_SizeGadget | #PB_Window_Tool | #PB_Window_Invisible)
			WndTree_list = TreeGadget(#PB_Any, TREE_WND_OFFSETX, TREE_WND_OFFSETY, WindowWidth(WndTree) - TREE_WND_OFFSETX*2, WindowHeight(WndTree) - TREE_WND_OFFSETY*2, #PB_Tree_AlwaysShowSelection)
			WindowBounds(WndTree, TREE_WND_WIDTH, TREE_WND_HEIGHT, TREE_WND_WIDTH*2, #PB_Ignore)
			SetGadgetFont(WndTree_list, FontID(TreeViewFont))
			Protected.l Styles = GetWindowLong_(GadgetID(WndTree_list), #GWL_STYLE)
			Styles = Styles | #TVS_EDITLABELS | #TVS_TRACKSELECT
			Styles = Styles & ~ #TVS_DISABLEDRAGDROP
			SetWindowLong_(GadgetID(WndTree_list), #GWL_STYLE, Styles)
			lpPrevWndFunc = SetWindowLong_(GadgetID(WndTree_list), #GWL_WNDPROC, @Tree_listMsg())
			BindEvent(#PB_Event_CloseWindow, @WndTreeClose(), WndTree)
			BindEvent(#PB_Event_Gadget, @TreeMenuRaise(), WndTree, WndTree_list, #PB_EventType_RightClick)
			BindEvent(#PB_Event_Gadget, @TreeItemChangedCB(), WndTree, WndTree_list, #PB_EventType_Change)
			CompilerIf Defined(IS_FULL, #PB_Constant)  ; this is full version (interface tree drag&drop files)
				EnableWindowDrop(WndTree, #PB_Drop_Files, #PB_Drag_Copy | #PB_Drag_Move | #PB_Drag_Link)
				BindEvent(#PB_Event_WindowDrop, @DropXMLFile(), WndTree)
			CompilerEndIf
			BindEvent(#PB_Event_SizeWindow, @WndTreeSize(), WndTree)
			BindEvent(#PB_Event_Timer, @WndTreeSizeTmr(), WndTree, WNDTREE_SIZETMR)
			AddWindowTimer(WndTree, WNDTREE_SIZETMR, 24)
			SetWindowCallback(@WndTreeMsg(), WndTree)
		EndProcedure

	;}

	; This is called when active item in treeview list changes
	Procedure TreeItemChangedCB()
		Protected TWindow = TreeSelectedWindow()
		If TWindow <> WndViewGet()
			WndViewSet(TWindow)
		EndIf
		WndParamsList (TreeSelectedObject())
	EndProcedure

	;{ Tree Item Actions logic }

		Structure TreeLogicData
			*Parent.OBJECT
			*ROOT.OBJECT
			isContainer.a		; This flag is true if item is container, but is not ROOT item
			isFull.a			; If item is container and can't have more childs, this flag is true
			isPanel.a			; This flag is true if item is PANEL (can contain <tab> items only)
			isRoot.a			; Is true if item is root (can't have any childs except windows)
			isRootEmpty.a		; This flag is true if dialog is empty (root item has 0 childs)
			isLocked.a			; Is true if item can't be renamed or deleted (root item can't be)
			childCount.i		; number of childs this item has in tree
		EndStructure

		; Returns set of logical flags describing currently selected tree node
		; RETURN:		true on success, false else
		Procedure TreeLogicData (*DAT.TreeLogicData)
			With *DAT
				\Parent = TreeSelectedObject()
				If FindMapElement(Tree(), #C_DIALOGS)
					\ROOT = @Tree(#C_DIALOGS)
				EndIf
				If \Parent = 0 Or \ROOT = 0
					ClearStructure(*DAT, TreeLogicData)
					ProcedureReturn #False
				EndIf
				\isContainer = Bool(\Parent <> \ROOT And IsContainer(\Parent\pEXTRA\Type$))
				\childCount = TreeGetChildCount(Tree(), \Parent\treeName$)
				\isFull = Bool(((\Parent\pEXTRA\Type$ = #C_SINGLEBOX Or
					\Parent\pEXTRA\Type$ = #C_CONTAINER Or
					\Parent\pEXTRA\Type$ = #C_FRAME Or
					\Parent\pEXTRA\Type$ = #C_TAB Or
					\Parent\pEXTRA\Type$ = #C_WINDOW Or
					\Parent\pEXTRA\Type$ = #C_SCROLLAREA) And
					\childCount >= 1) Or
					(\Parent\pEXTRA\Type$ = #C_SPLITTER And \childCount >= 2))
				\isPanel = Bool(\Parent\pEXTRA\Type$ = #C_PANEL)
				\isRoot = Bool(\Parent = \ROOT)
				\isRootEmpty = Bool(TreeGetChildCount(Tree(), #C_DIALOGS) = 0)
				\isLocked = Bool(\Parent = \ROOT)
			EndWith
			ProcedureReturn #True
		EndProcedure

		; This is tree items logic, used in drag&drop and tree menu
		; Assuming target is a container here, and start is item which should be inserted into it
		; RETURN:		true if start can be placed into target, false else
		Procedure TreeLogicCanPaste_raw (*Start.OBJECT, *Target.OBJECT)
			Protected FuckOff ; if set to true, start can't be added to target
			If *Start\pEXTRA\type$ = #C_TAB And Not *Target\pEXTRA\type$ = #C_PANEL
				FuckOff = #True
			EndIf
			If *Target\pEXTRA\type$ = #C_PANEL And Not *Start\pEXTRA\type$ = #C_TAB
				FuckOff = #True
			EndIf
			If *Start\pEXTRA\type$ = #C_WINDOW And Not *Target\pEXTRA\type$ = #C_DIALOGS
				FuckOff = #True
			EndIf
			If *Target\pEXTRA\type$ = #C_DIALOGS And Not *Start\pEXTRA\type$ = #C_WINDOW
				FuckOff = #True
			EndIf
			Select *Target\pEXTRA\type$
				Case #C_SINGLEBOX, #C_CONTAINER, #C_FRAME, #C_TAB, #C_WINDOW, #C_SCROLLAREA
					If TreeGetChildCount(Tree(), *Target\treeName$) >= 1
						FuckOff = #True
					EndIf
				Case #C_SPLITTER:
					If TreeGetChildCount(Tree(), *Target\treeName$) >= 2
						FuckOff = #True
					EndIf
				Case #C_PANEL:
					If Not *Start\pEXTRA\type$ = #C_TAB
						FuckOff = #True
					EndIf
			EndSelect
			ProcedureReturn Bool(Not FuckOff)
		EndProcedure

		; Common logic
		; Every function here returns True on success
		; can selected item be renamed?
		Procedure TreeLogicCanRename(*Dat.TreeLogicData)
			ProcedureReturn Bool(Not *Dat\isLocked)
		EndProcedure
		; can item be deleted?
		Procedure TreeLogicCanDelete(*Dat.TreeLogicData)
			ProcedureReturn Bool(Not *Dat\isLocked)
		EndProcedure
		; can item be copied with all subitems?
		Procedure TreeLogicCanCopy(*Dat.TreeLogicData)
			ProcedureReturn Bool(Not *Dat\isRoot)
		EndProcedure
		; can item be cut with subitems?
		Procedure TreeLogicCanCut(*Dat.TreeLogicData)
			ProcedureReturn Bool(Not *Dat\isRoot And Not *Dat\isLocked)
		EndProcedure
		; is file export (code, xml) possible?
		Procedure TreeLogicCanExport()
			ProcedureReturn Bool(Not TreeGetChildCount(Tree(), #C_DIALOGS) = 0)
		EndProcedure
		; can copied item be pasted here?
		; *CopyObj			copied object to paste
		Procedure TreeLogicCanPaste(*Dat.TreeLogicData, *CopyObj.OBJECT)
			ProcedureReturn Bool(*CopyObj <> 0 And 										; if there is some obj copied
				((*Dat\isContainer And Not *Dat\isFull And TreeLogicCanPaste_raw(*CopyObj, *Dat\Parent)) Or 		; and target is container, not full, and logic is OK
				(*Dat\isRoot And *CopyObj\pEXTRA\type$ = #C_WINDOW)))							; or copied object=window and target=dialogs
		EndProcedure

	;}

	;{ Tree Drag&Drop }

		Global IsDrag			; if non-zero, some tree item is currently dragged
		Global DragHwnd			; handle of a dragged item
		; *pvdi2		LParam value received from #TVN_BEGINDRAG message
		Procedure TreeDragStart (*pvdi2.NMTREEVIEW)
			Protected res = #PB_ProcessPureBasicEvents
			If Not IsDrag
				Protected Tree_hwnd = GadgetID(WndTree_list)
				Protected TreeParent_hwnd = GetParent_(Tree_hwnd)
				Protected hImg = SendMessage_(Tree_hwnd, #TVM_CREATEDRAGIMAGE, 0, *pvdi2\itemNew\hItem)
				If hImg
					ImageList_BeginDrag_(hImg, 0, 0, 0)
					ImageList_DragEnter_(Tree_hwnd, *pvdi2\ptDrag\x, *pvdi2\ptDrag\y);
					ShowCursor_(0)
					SetCapture_(TreeParent_hwnd)
					ImageList_Destroy_(hImg)
					DragHwnd = *pvdi2\itemNew\hItem
				EndIf
				IsDrag = #True
				res = 0
			EndIf
			ProcedureReturn res
		EndProcedure
		; LParam		current X, Y mouse coordinates
		Procedure TreeDrag (LParam)
			Protected res = #PB_ProcessPureBasicEvents
			If IsDrag
				Protected Tree_hwnd = GadgetID(WndTree_list)
				Protected TreeParent_hwnd = GetParent_(Tree_hwnd)
				Protected cur.POINT
				cur\x = PeekW(@LParam)
				cur\y = PeekW(@LParam + 2)
				ClientToScreen_(TreeParent_hwnd, @cur)
				ScreenToClient_(Tree_hwnd, @cur)
				ImageList_DragMove_(cur\x, cur\y)
				ImageList_DragShowNolock_(#False)
				Protected c.TV_HITTESTINFO
				c\pt\x = cur\x
				c\pt\y = cur\y
				Protected TargetItem = SendMessage_(Tree_hwnd, #TVM_HITTEST, 0, @c)
				If TargetItem
					SendMessage_(Tree_hwnd, #TVM_SELECTITEM, #TVGN_DROPHILITE, TargetItem)
				EndIf
				ImageList_DragShowNolock_(#True)
			EndIf
			ProcedureReturn res
		EndProcedure

		Procedure TreeDragEnd ()
			Protected res = #PB_ProcessPureBasicEvents
			If IsDrag
				Protected Tree_hwnd = GadgetID(WndTree_list)
				IsDrag = #False
				Protected TargetHwnd = SendMessage_(Tree_hwnd, #TVM_GETNEXTITEM, #TVGN_DROPHILITE, 0)
				If TargetHwnd
					Protected *Start.OBJECT =  TreeHItemToData(WndTree_list, DragHwnd)		; the dragged object
					Protected *Target.OBJECT =  TreeHItemToData(WndTree_list, TargetHwnd)	; target object
					Protected FuckOff = #False												; a flag to control movement. if set to true, movement is aborted
					Protected HasMoved = #False
					If *Start And *Target And Not (*Start = *Target) And Not (*Start\pEXTRA\type$ = #C_DIALOGS) And Not (*Target\pEXTRA\type$ = #C_DIALOGS)
						Protected Order, IntoContainer
						Protected StartParent$ = Tree(*Start\treeName$)\Parent$
						Protected FinParent$ = Tree(*Target\treeName$)\Parent$
						Protected *TargetParent.OBJECT = @Tree(FinParent$)
						IntoContainer = IsContainer(*Target\pEXTRA\Type$)
						If IntoContainer And Not *Start\pEXTRA\type$ = #C_WINDOW And Not (StartParent$ = *Target\treeName$) And TreeGetChildCount(Tree(), *Target\treeName$) = 0
							FuckOff = Bool(Not TreeLogicCanPaste_raw(*Start, *Target))
							If Not FuckOff
								Order = #PB_List_Last
								If StartParent$ = *Target\treeName$
									Order = #PB_List_First
									If TreeGetChildCount(Tree(), *Target\treeName$) > 0
										FirstElement(Tree(*Target\treeName$)\Childs$())
										If Tree(*Target\treeName$)\Childs$() = *Start\treeName$
											FuckOff = #True
										EndIf
									EndIf
								EndIf
							EndIf
						Else
							IntoContainer = #False
							Order = #PB_List_After
							If StartParent$ = FinParent$
								If TreeGetChildCount(Tree(), StartParent$) < 2
									FuckOff = #True
								Else
									Protected fCn, lCn
									ForEach Tree(StartParent$)\Childs$()
										If Tree(StartParent$)\Childs$() = *Target\treeName$
											lCn = ListIndex(Tree(StartParent$)\Childs$())
										EndIf
										If Tree(StartParent$)\Childs$() = *Start\treeName$
											fCn = ListIndex(Tree(StartParent$)\Childs$())
										EndIf
									Next
									If fCn >= lCn
										Order = #PB_List_Before
									Else
										Order = #PB_List_After
									EndIf
								EndIf
							Else
								Select *TargetParent\pEXTRA\type$
									Case #C_SINGLEBOX, #C_CONTAINER, #C_FRAME, #C_TAB, #C_WINDOW, #C_SCROLLAREA
										If TreeGetChildCount(Tree(), *TargetParent\treeName$) >= 1
											FuckOff = #True
										EndIf
									Case #C_SPLITTER:
										If TreeGetChildCount(Tree(), *TargetParent\treeName$) >= 2
											FuckOff = #True
										EndIf
									Case #C_PANEL:
										If Not *Start\pEXTRA\type$ = #C_TAB
											FuckOff = #True
										EndIf
								EndSelect
							EndIf
							If *Start\pEXTRA\type$ = #C_WINDOW
								IntoContainer = #False
								If Not *TargetParent\pEXTRA\type$ = #C_DIALOGS
									FuckOff = #True
								EndIf
							EndIf
							If *TargetParent\pEXTRA\type$ = #C_DIALOGS And Not *Start\pEXTRA\type$ = #C_WINDOW
								FuckOff = #True
							EndIf
							If *Start\pEXTRA\type$ = #C_TAB And Not *TargetParent\pEXTRA\type$ = #C_PANEL
								FuckOff = #True
							EndIf
						EndIf
						If Not FuckOff
							HasMoved = TreeMove(Tree(), *Start\treeName$, *Target\treeName$, IntoContainer, Order)
						EndIf
					EndIf
				EndIf
				ImageList_EndDrag_()
				ReleaseCapture_()
				ShowCursor_(#True)
				Protected NewMap ExpandedItems()
				TreeExpandedNames(ExpandedItems())
				If *Target
					ExpandedItems(LCase(Tree(*Target\treeName$)\Parent$)) = #True
					If IntoContainer
						ExpandedItems(LCase(*Target\treeName$)) = #True
					EndIf
				EndIf
				If *Start
					RedrawTree(ExpandedItems(), *Start\treeName$)
				Else
					RedrawTree(ExpandedItems(), "")
				EndIf
				RedrawWindow_(Tree_hwnd, 0, 0, #RDW_ERASE)
				WndParamsList(TreeSelectedObject())
				WndViewSet(TreeSelectedWindow())
				DragHwnd = 0
				res = 0
			EndIf
			ProcedureReturn res
		EndProcedure

	;}

	;{ Tree Label Edit / Rename }

		; This is flag function for node rename
		; Allow			if non-zero, then label edit will be enabled for next function call
		; RETURN:		true to allow label edit, false else
		Procedure TreeEditAllow(Allow = #False)
			Static State
			If Allow
				State = #True
			ElseIf State
				State = #False
				ProcedureReturn #True
			Else
				ProcedureReturn #False
			EndIf
		EndProcedure

		; Starts node rename operation
		; RETURN:		none
		Procedure TreeEditInit()
			Protected CNode.TreeLogicData
			If TreeLogicData(CNode) = #False Or TreeLogicCanRename(CNode) = #False
				ProcedureReturn
			EndIf
			Protected Current2 = GetGadgetState(WndTree_list)
			If Not Current2 = -1
				TreeEditAllow(#True)
				PostMessage_(GadgetID(WndTree_list), #TVM_EDITLABEL, 0, GadgetItemID(WndTree_list, Current2))
			EndIf
		EndProcedure

		; And following two are callbacks from #TVN_BEGINLABELEDIT and #TVN_ENDLABELEDIT
		; - *pvdi		LParam value of message
		; - RETURN:		return true from those procedures to abort edit operation on current stage
		Procedure TreeEditStart (*pvdi.NMTVDISPINFO)
			Protected result = #PB_ProcessPureBasicEvents
			If Not TreeEditAllow()
				result = #True
			EndIf
			If Not result And *pvdi\item\pszText And (LCase(PeekS(*pvdi\item\pszText)) = #C_DIALOGS)
				result = #True
			EndIf
			ProcedureReturn result
		EndProcedure

		Procedure TreeEditEnd (*pvdi.NMTVDISPINFO)
			Protected.a Res = #False
			If *pvdi\item\pszText
				Protected *C.OBJECT = TreeSelectedObject()
				Protected *N.TreeNode
				Protected NewName$ = PeekS(*pvdi\item\pszText)
				Protected NewTreeName$ = LCase(NewName$)
				If *C
					If (IsValueSupported(*C\pEXTRA\type$, #v_onevent) Or IsValueSupported(*C\pEXTRA\type$, #v_variable) Or *C\pEXTRA\type$ = #C_WINDOW) And Not isNameValid(NewTreeName$)
						Res = #False
					ElseIf TreeRename(Tree(), *C\treeName$, NewTreeName$) Or (NewTreeName$ = *C\treeName$ And Not NewName$ = *C\pCOMMON\name$)
						*N = @Tree(NewTreeName$)
						*N\Data\treeName$ = NewTreeName$
						*N\Data\pCOMMON\name$ = NewName$
						SetGadgetItemData(WndTree_list, GetGadgetState(WndTree_list), *N)
						WndViewSet(TreeSelectedWindow())
						WndParamsList (TreeSelectedObject())
						RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
						Res = #True
					EndIf
				EndIf
				ProcedureReturn Res
			EndIf
		EndProcedure

	;}

	;{ Tree Add/Delete node }

		; Called to create new node in tree as a child of a currently selected one
		; Type$			one of #C_ constants
		; RETURN:		none
		Procedure TreeAddNode(Type$)
			Protected Parent$ = TreeSelectedName()
			Protected *New.OBJECT
			If Parent$ And Type$ And Not GetGadgetState(WndTree_list) = -1
				*New = objNew(Parent$, Type$, "", 0, #PB_List_Last, #True)
				If *New
					Protected NewMap Expanded()
					TreeExpandedNames(Expanded())
					RedrawTree(Expanded(), *New\pCOMMON\name$)
					RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
					WndParamsList(TreeSelectedObject())
					WndViewSet(TreeSelectedWindow())
				EndIf
			EndIf
		EndProcedure

		; Deletes current node both from treeview control and main tree
		; IgnoreConfirm		if true, no delete confirmation is shown (even if option enabled in cfg)
		Procedure TreeDeleteNode(IgnoreConfirm)
			Protected CNode.TreeLogicData
			If TreeLogicData(CNode) = #False Or TreeLogicCanDelete(CNode) = #False
				ProcedureReturn
			EndIf
			Protected Name$ = TreeSelectedName()
			Protected Current1 = GetGadgetState(WndTree_list)
			If Name$ And Not Current1 = -1 And Not Tree(Name$)\Data\pEXTRA\type$ = #C_DIALOGS
				If Not IgnoreConfirm And cfg\ConfirmDelete And MessageRequester("Delete item (" + Name$ + ")", "Delete selected object?", #PB_MessageRequester_Info | #PB_MessageRequester_YesNo) = #PB_MessageRequester_No
					ProcedureReturn
				EndIf
				If TreeDelete(Tree(), Name$, #True)
					RemoveGadgetItem(WndTree_list, Current1)
					WndViewSet(TreeSelectedWindow())
					WndParamsList (TreeSelectedObject())
				EndIf
			EndIf
		EndProcedure

	;}

	;{ Tree Cut/Copy/Paste operations }

		Global NewList CBuf.TreeNode()			; copied items data
		Global NewMap CNames$()					; name map of copied items
		Global *CopyObj.OBJECT					; the root item of copied items (pointer to a CBuf(0) item)
		; Copies current node and all nested items to a buffer
		; RETURN:		none
		Procedure CopyCB(*ITEM.TreeNode)
			AddElement(CBuf())
			CopyStructure(*ITEM, @CBuf(), TreeNode)
			CNames$(*ITEM\Data\treeName$) = *ITEM\Data\treeName$
		EndProcedure
		Procedure TreeCopyNode()
			Protected CNode.TreeLogicData
			If TreeLogicData(CNode) = #False Or TreeLogicCanCopy(CNode) = #False
				ProcedureReturn
			EndIf
			Protected *TObj.OBJECT = TreeSelectedObject()
			*CopyObj = 0
			ClearList(CBuf())
			ClearMap(CNames$())
			If *TObj
				TreeRecurse(Tree(), *TObj\treeName$, @CopyCB())
				FirstElement(CBuf())
				*CopyObj = @CBuf()
			EndIf
		EndProcedure
		; Pastes items from a buffer to a current tree position (adding them as a childs of current item)
		; RETURN:		none
		Procedure TreePasteNode()
			Protected CNode.TreeLogicData
			If TreeLogicData(CNode) = #False Or TreeLogicCanPaste(CNode, *CopyObj) = #False
				ProcedureReturn
			EndIf
			Protected *TObj.OBJECT = TreeSelectedObject()
			Protected *New.OBJECT
			Protected Target$
			Protected NewMap tCName$()
			If *TObj And ListSize(CBuf()) >= 1
				CopyMap(CNames$(), tCName$())
				ResetList(CBuf())
				ForEach CBuf()
					If *New = 0
						*New = objNew(Tree(*TObj\treeName$)\Data\treeName$, CBuf()\Data\pEXTRA\type$, CBuf()\Data\pCOMMON\name$, @CBuf()\Data, #PB_List_Last, #True)
						tCName$(CBuf()\Data\treeName$) = *New\treeName$
						Target$ = *New\treeName$
					Else
						*New = objNew(tCName$(CBuf()\Parent$), CBuf()\Data\pEXTRA\type$, CBuf()\Data\pCOMMON\name$, @CBuf()\Data, #PB_List_Last, #True)
						tCName$(CBuf()\Data\treeName$) = *New\treeName$
					EndIf
				Next
				Protected NewMap Expanded()
				TreeExpandedNames(Expanded())
				RedrawTree(Expanded(), Target$)
				RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
				WndParamsList(TreeSelectedObject())
				WndViewSet(TreeSelectedWindow())
			EndIf
		EndProcedure

		Procedure TreeCutNode()
			Protected CNode.TreeLogicData
			If TreeLogicData(CNode) = #False Or TreeLogicCanCut(CNode) = #False
				ProcedureReturn
			EndIf
			TreeCopyNode()
			TreeDeleteNode(#True)
		EndProcedure

		; Returns *CopyObj value
		Procedure TreeCopiedNode()
			ProcedureReturn *CopyObj
		EndProcedure
	;}

	;{ Tree Expand/Collapse/Refresh }

		Global NewMap ExpansionMap()
		Procedure TreeExpandCB (*ITEM.TreeNode)
			ExpansionMap(LCase(*ITEM\Data\pCOMMON\name$)) = #True
		EndProcedure

		; Current$		an item to start from (must be "treename")
		; Collapse		if true, nested items are collapsed. else, expanded
		; RETURN:		none
		Procedure TreeExpandCollapseNested(Current$, Collapse)
			Current$ = LCase(Current$)
			If Current$ And FindMapElement(Tree(), Current$)
				Protected NewMap Expanded()
				TreeExpandedNames(Expanded())
				TreeRecurse(Tree(), Current$, @TreeExpandCB())
				If Collapse = #False
					ForEach ExpansionMap()
						Expanded(MapKey(ExpansionMap())) = #True
					Next
				Else
					ForEach ExpansionMap()
						If FindMapElement(Expanded(), MapKey(ExpansionMap()))
							DeleteMapElement(Expanded())
						EndIf
					Next
				EndIf
				RedrawTree(Expanded(), Current$)
				RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
				ClearMap(ExpansionMap())
			EndIf
		EndProcedure

		; Simply trigger whole tree refresh
		; RETURN:		none
		Procedure TreeRefresh()
			Protected NewMap Expanded()
			TreeExpandedNames(Expanded())
			RedrawTree(Expanded(), TreeSelectedName())
			RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
			WndParamsList(TreeSelectedObject())
			WndViewSet(TreeSelectedWindow())
		EndProcedure

	;}

	;{ Tree Redraw }

		; Those 2 functions are cleaning and completely redrawing tree control
		; This is just callback from Tree() code, it simply adds items to a control
		Procedure RedrawCB(*ITEM.TreeNode)
			Protected Type$ = *ITEM\Data\pEXTRA\type$
			AddGadgetItem(WndTree_list, -1, *ITEM\Data\pCOMMON\name$, TreeCargoIco(Type$), TreeGetLevel(Tree(), *ITEM\Data\treeName$))
			SetGadgetItemData(WndTree_list, CountGadgetItems(WndTree_list) - 1, @*ITEM\Data)
		EndProcedure
		; This is procedure to launch redraw
		; ExpandedItems()		a map of item names to set expanded in treeview. map keys must be lowercased, except "WhoWantsToLiveForever" - if this key is present, all items are expanded
		; SelectedItem$			item name to set currently selected in treeview
		; RETURN:				none
		Procedure RedrawTree(Map ExpandedItems(), SelectedItem$)
			LockWindowUpdate_(GadgetID(WndTree_list))
			Protected VSc = GetScrollPos_(GadgetID(WndTree_list), #SB_VERT)
			Protected CItem$
			ClearGadgetItems(WndTree_list)
			Protected.q speed = ElapsedMilliseconds()
			TreeRecurse(Tree(), #C_DIALOGS, @RedrawCB())
			Protected expand_all = FindMapElement(ExpandedItems(), "WhoWantsToLiveForever")
			Protected t
			Protected style
			SelectedItem$ = LCase(SelectedItem$)
			For t = 0 To CountGadgetItems(WndTree_list)
				style = 0
				CItem$ = LCase(GetGadgetItemText(WndTree_list, t))
				If expand_all Or FindMapElement(ExpandedItems(), CItem$)
		 			style | #PB_Tree_Expanded
				EndIf
				If CItem$ = SelectedItem$
					style | #PB_Tree_Selected
					SetGadgetState(WndTree_list, t)
				EndIf
				If style
					SetGadgetItemState(WndTree_list, t, style)
				EndIf
			Next t
			SetScrollPos_(GadgetID(WndTree_list), #SB_VERT, VSc, #False)
			LockWindowUpdate_(0)
			Debug "[delay] RedrawTree: " + Str(ElapsedMilliseconds() - speed)
		EndProcedure

	;}

	; returns pointer to a Tree() element of type OBJECT, which is attached to a currently selected node
	; RETURN:		pointer to OBJECT structure or 0
	Procedure TreeSelectedObject()
		Protected TItem = GetGadgetState(WndTree_list)
		If Not TItem = -1
			ProcedureReturn GetGadgetItemData(WndTree_list, TItem)
		EndIf
	EndProcedure
	; returns pointer to a Tree() element of type OBJECT. Returned element is "window", that window is parent of currently selected node
	; RETURN:		pointer to OBJECT structure or 0
	Procedure TreeSelectedWindow()
		Protected TItem = GetGadgetState(WndTree_list)
		Protected *T.OBJECT
		Protected *P.TreeNode
		Protected Parent$
		If Not TItem = -1
			*T = GetGadgetItemData(WndTree_list, TItem)
			If *T\pEXTRA\Type$ = #C_WINDOW
				ProcedureReturn *T
			EndIf
			*P = @Tree(*T\treeName$)
			Parent$ = *P\Parent$
			Repeat
				If Parent$ And FindMapElement(Tree(), Parent$)
					If Tree()\Data\pEXTRA\Type$ = #C_WINDOW
						*T = @Tree()\Data
						Break
					Else
						Parent$ = Tree()\Parent$
					EndIf
				Else
					*T = 0
					Break
				EndIf
			ForEver
		EndIf
		ProcedureReturn *T
	EndProcedure
	; returns "treename" of currently selected node (used to reference a Tree() map)
	; RETURN:		string with object treename, or empty string
	Procedure$ TreeSelectedName()
		Protected TItem = GetGadgetState(WndTree_list)
		Protected *Item.OBJECT
		If Not TItem = -1
			*Item = GetGadgetItemData(WndTree_list, TItem)
			If *Item
				ProcedureReturn *Item\treeName$
			EndIf
		EndIf
	EndProcedure

	; Returns list of all tree nodes which have expanded state
	; RETURN:		none, Out() map contains keys representing expanded items (lowercased)
	Procedure TreeExpandedNames(Map Out())
		Protected x = CountGadgetItems(WndTree_list)
		ClearMap(Out())
		While x >= 0
			If Bool(GetGadgetItemState(WndTree_list, x) & #PB_Tree_Expanded)
				Out(LCase(GetGadgetItemText(WndTree_list, x))) = #True
			EndIf
			x-1
		Wend
	EndProcedure

	; Converts treeview item OS handle to a OBJECT pointer attached to this item
	; Control		PB treeview control
	; hwnd			OS handle to treeview item
	; RETURN:		item data value on success, -1 else
	Procedure TreeHItemToData (Control, hwnd)
		Protected c = CountGadgetItems(Control)
		Protected item = -1
		While c >= 0
			If GadgetItemID(Control, c) = hwnd
				item = GetGadgetItemData(Control, c)
				Break
			EndIf
			c - 1
		Wend
		ProcedureReturn item
	EndProcedure

;}

;{ UI 	: tree menu }

	Enumeration #PB_Compiler_EnumerationValue
		#MENU_TREE__MENU	; menu start index. also used as menu ID. should not assign some menu items to it
		#MENU_TREE_DELETE
		#MENU_TREE_RENAME
		#MENU_TREE_CUT
		#MENU_TREE_COPY
		#MENU_TREE_PASTE
		#MENU_TREE_EXPORT_XMLCODE_EX
		#MENU_TREE_FILE_SAVE
		#MENU_TREE_FILE_LOAD
		#MENU_TREE_REFRESH
		#MENU_TREE_NEW_WINDOW
		#MENU_TREE_NEW_HBOX
		#MENU_TREE_NEW_VBOX
		#MENU_TREE_NEW_GBOX
		#MENU_TREE_NEW_MBOX
		#MENU_TREE_NEW_SBOX
		#MENU_TREE_NEW_CONTAINER
		#MENU_TREE_NEW_SCROLAREA
		#MENU_TREE_NEW_FRAME
		#MENU_TREE_NEW_SPLITTER
		#MENU_TREE_NEW_PANEL
		#MENU_TREE_NEW_BUTTON
		#MENU_TREE_NEW_BUTTONIMG
		#MENU_TREE_NEW_CHECKBOX
		#MENU_TREE_NEW_OPTIONBOX
		#MENU_TREE_NEW_RTF
		#MENU_TREE_NEW_STRING
		#MENU_TREE_NEW_TEXT
		#MENU_TREE_NEW_SCINTILLA
		#MENU_TREE_NEW_IPADDRESS
		#MENU_TREE_NEW_LINK
		#MENU_TREE_NEW_CANVAS
		#MENU_TREE_NEW_IMAGE
		#MENU_TREE_NEW_COMBO
		#MENU_TREE_NEW_LISTICON
		#MENU_TREE_NEW_LISTVIEW
		#MENU_TREE_NEW_TREE
		#MENU_TREE_NEW_COMBOEXPLORER
		#MENU_TREE_NEW_LISTEXPLORER
		#MENU_TREE_NEW_TREEEXPLORER
		#MENU_TREE_NEW_SCROLBAR
		#MENU_TREE_NEW_TRACKBAR
		#MENU_TREE_NEW_SPIN
		#MENU_TREE_NEW_DATE
		#MENU_TREE_NEW_CALENDAR
		#MENU_TREE_NEW_PROGRESSBAR
		#MENU_TREE_NEW_WEB
		#MENU_TREE_NEW_TAB
		#MENU_TREE_NEW_EMPTY
		#MENU_TREE_CFG_EXPORT2CB
		#MENU_TREE_CFG_NOCAPTION
		#MENU_TREE_CFG_CONFIRMCHANGES
		#MENU_TREE_CFG_RUNCODE
		#MENU_TREE_CFG_DPIPATCH
		#MENU_TREE_CFG_CONFIRMDELETE
		#MENU_TREE_CFG_NOLOOP
		#MENU_TREE_EXPAND
		#MENU_TREE_COLLAPSE
		#MENU_TREE__END	; menu last index, should not assign menu items to it too
	EndEnumeration

	; Convert menu #MENU_TREE_NEW_ identifier to a #C_ object type
	; RETURN:		#C_ constant string or empty string
	Procedure$ TreeMenuGetType(MAIN_NEW_id)
		Protected res$
		Select MAIN_NEW_id
			Case #MENU_TREE_NEW_WINDOW: 		res$ = #C_WINDOW
			Case #MENU_TREE_NEW_HBOX: 			res$ = #C_HBOX
			Case #MENU_TREE_NEW_VBOX: 			res$ = #C_VBOX
			Case #MENU_TREE_NEW_GBOX: 			res$ = #C_GRIDBOX
			Case #MENU_TREE_NEW_MBOX: 			res$ = #C_MULTIBOX
			Case #MENU_TREE_NEW_SBOX: 			res$ = #C_SINGLEBOX
			Case #MENU_TREE_NEW_CONTAINER: 		res$ = #C_CONTAINER
			Case #MENU_TREE_NEW_SCROLAREA: 		res$ = #C_SCROLLAREA
			Case #MENU_TREE_NEW_FRAME: 			res$ = #C_FRAME
			Case #MENU_TREE_NEW_SPLITTER: 		res$ = #C_SPLITTER
			Case #MENU_TREE_NEW_PANEL: 			res$ = #C_PANEL
			Case #MENU_TREE_NEW_BUTTON: 		res$ = #C_BUTTON
			Case #MENU_TREE_NEW_BUTTONIMG: 		res$ = #C_BUTTONIMAGE
			Case #MENU_TREE_NEW_CHECKBOX: 		res$ = #C_CHECKBOX
			Case #MENU_TREE_NEW_OPTIONBOX: 		res$ = #C_OPTION
			Case #MENU_TREE_NEW_RTF: 			res$ = #C_EDITOR
			Case #MENU_TREE_NEW_STRING: 		res$ = #C_STRING
			Case #MENU_TREE_NEW_TEXT: 			res$ = #C_TEXT
			Case #MENU_TREE_NEW_SCINTILLA: 		res$ = #C_SCINTILLA
			Case #MENU_TREE_NEW_IPADDRESS: 		res$ = #C_IPADDRESS
			Case #MENU_TREE_NEW_LINK: 			res$ = #C_HYPERLINK
			Case #MENU_TREE_NEW_CANVAS: 		res$ = #C_CANVAS
			Case #MENU_TREE_NEW_IMAGE: 			res$ = #C_IMAGE
			Case #MENU_TREE_NEW_COMBO: 			res$ = #C_COMBOBOX
			Case #MENU_TREE_NEW_LISTICON: 		res$ = #C_LISTICON
			Case #MENU_TREE_NEW_LISTVIEW: 		res$ = #C_LISTVIEW
			Case #MENU_TREE_NEW_TREE: 			res$ = #C_TREE
			Case #MENU_TREE_NEW_COMBOEXPLORER: 	res$ = #C_EXPLORERCOMBO
			Case #MENU_TREE_NEW_LISTEXPLORER: 	res$ = #C_EXPLORERLIST
			Case #MENU_TREE_NEW_TREEEXPLORER: 	res$ = #C_EXPLORERTREE
			Case #MENU_TREE_NEW_SCROLBAR: 		res$ = #C_SCROLLBAR
			Case #MENU_TREE_NEW_TRACKBAR: 		res$ = #C_TRACKBAR
			Case #MENU_TREE_NEW_SPIN: 			res$ = #C_SPIN
			Case #MENU_TREE_NEW_DATE: 			res$ = #C_DATE
			Case #MENU_TREE_NEW_CALENDAR: 		res$ = #C_CALENDAR
			Case #MENU_TREE_NEW_PROGRESSBAR: 	res$ = #C_PROGRESSBAR
			Case #MENU_TREE_NEW_WEB: 			res$ = #C_WEB
			Case #MENU_TREE_NEW_TAB: 			res$ = #C_TAB
			Case #MENU_TREE_NEW_EMPTY: 			res$ = #C_EMPTY
		EndSelect
		ProcedureReturn res$
	EndProcedure

	; Callback procedure for tree list menu
	Procedure TreeMenuCB()
		Select EventMenu()
			Case #MENU_TREE__MENU To #MENU_TREE__END : ; this check is required only if few different menus binded to one callback function
				Select EventMenu()
					Case #MENU_TREE_NEW_WINDOW To #MENU_TREE_NEW_EMPTY:
						TreeAddNode(TreeMenuGetType (EventMenu()))
					Case #MENU_TREE_CUT:
						TreeCutNode()
					Case #MENU_TREE_DELETE:
						TreeDeleteNode(#False)
					Case #MENU_TREE_COPY:
						TreeCopyNode()
					Case #MENU_TREE_PASTE:
						TreePasteNode()
					Case #MENU_TREE_RENAME:
						TreeEditInit()
					Case #MENU_TREE_EXPAND:
						TreeExpandCollapseNested(TreeSelectedName(), #False)
					Case #MENU_TREE_COLLAPSE:
						TreeExpandCollapseNested(TreeSelectedName(), #True)
					Case #MENU_TREE_REFRESH:
						TreeRefresh()
					Case #MENU_TREE_CFG_EXPORT2CB:
						cfg\Export2Clipboard = Bool(Not cfg\Export2Clipboard)
					Case #MENU_TREE_CFG_NOCAPTION:
						cfg\NoCaptions = Bool(Not cfg\NoCaptions)
					Case #MENU_TREE_CFG_CONFIRMCHANGES:
						cfg\ConfirmChanges = Bool(Not cfg\ConfirmChanges)
					Case #MENU_TREE_CFG_CONFIRMDELETE:
						cfg\ConfirmDelete = Bool(Not cfg\ConfirmDelete)
					Case #MENU_TREE_CFG_RUNCODE:
						cfg\RunCode = Bool(Not cfg\RunCode)
					Case #MENU_TREE_CFG_DPIPATCH:
						cfg\DPIPatch = Bool(Not cfg\DPIPatch)
						WndViewSet(TreeSelectedWindow())
					Case #MENU_TREE_CFG_NOLOOP:
						cfg\NoLoop = Bool(Not cfg\NoLoop)
					Case #MENU_TREE_EXPORT_XMLCODE_EX:
						SaveCodeFile(cfg\Export2Clipboard)
					Case #MENU_TREE_FILE_SAVE:
						SaveXMLFile(cfg\Export2Clipboard)
					Case #MENU_TREE_FILE_LOAD:
						CompilerIf Defined(IS_FULL, #PB_Constant) ; this is full version (interface tree: load XML)
							OpenXMLFile()
						CompilerEndIf
				EndSelect
		EndSelect
	EndProcedure

	; Creates and displays Interface Tree menu
	; RETURN:		none
	Procedure TreeMenuRaise()
		If IsMenu(#MENU_TREE__MENU) : FreeMenu(#MENU_TREE__MENU) : EndIf
		;{ Menu Display Logic }

			Protected Current.TreeLogicData
			If TreeLogicData (Current) = #False
				ProcedureReturn
			EndIf
			Protected ChkRename = TreeLogicCanRename(Current)
			Protected ChkDelete = TreeLogicCanDelete(Current)
			Protected ChkCopy = TreeLogicCanCopy(Current)
			Protected ChkCut = TreeLogicCanCut(Current)
			Protected ChkCanExport = TreeLogicCanExport()
			Protected ChkPaste = TreeLogicCanPaste(Current, TreeCopiedNode())
			Protected chkNewCommon = Bool(Current\isContainer And Not Current\isFull And Not Current\IsPanel)
			Protected ChkNewTab = Bool(Current\isContainer And Not Current\isFull And Current\IsPanel)
			Protected ChkNewWindow = Bool(Current\isRoot)
			Protected ChkMsgFull = Bool(Not Current\isRoot And Current\isContainer And Current\isFull)
			Protected ChkMsgNotContainer = Bool(Not Current\isRoot And Not Current\isContainer)
			Protected ChkExpandCollapse = Bool(Current\childCount)
		;}

		If CreatePopupMenuEx(#MENU_TREE__MENU, @TreeMenuCB())
			;{ Create menu }
				Protected NewText$
				If ChkMsgFull
					NewText$ = "(container is full)"
				ElseIf ChkMsgNotContainer
					NewText$ = "(not a container)"
				Else
					NewText$ = "New"
				EndIf
				If OpenSubMenuEx (NewText$, ChkNewWindow | ChkNewCommon | ChkNewTab, #True)
					MenuItemEx (#MENU_TREE_NEW_WINDOW, "Window" + #TAB$ + "1", 				#True, ChkNewWindow)
					MenuBarEx(ChkNewWindow & (ChkNewCommon | ChkNewTab))
					If OpenSubMenuEx ("Container", 											#True, ChkNewCommon | ChkNewTab)
						MenuItemEx (#MENU_TREE_NEW_HBOX, "HBox", 							#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_VBOX, "VBox",							#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_GBOX, "GridBox", 						#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_SPLITTER, "Splitter" + #TAB$ + "2", 		#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_MBOX, "MultiBox", 						#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_SBOX, "SingleBox" + #TAB$ + "1", 		#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_CONTAINER, "Container" + #TAB$ + "1", 	#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_FRAME, 	"Frame" + #TAB$ + "1", 			#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_SCROLAREA, "ScrollArea" + #TAB$ + "1", 	#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_PANEL, "Panel", 							#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_TAB, "Tab", 								#True, ChkNewTab)
						CloseSubMenu()
					EndIf
					MenuBarEx(ChkNewCommon)
					If OpenSubMenuEx ("Button / Input", 									#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_BUTTON, "Button", 						#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_BUTTONIMG, "ButtonImage", 				#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_CHECKBOX, "CheckBox", 					#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_OPTIONBOX, "OptionBox", 					#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_SCROLBAR, "ScrollBar", 					#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_TRACKBAR, "TrackBar", 					#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_SPIN, "SpinBox", 						#True, ChkNewCommon)
						CloseSubMenu()
					EndIf
					If OpenSubMenuEx ("Image / Display", 									#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_CANVAS, "Canvas", 						#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_IMAGE, "Image", 							#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_PROGRESSBAR, "ProgressBar", 				#True, ChkNewCommon)
						CloseSubMenu()
					EndIf
					If OpenSubMenuEx ("Text / String", 										#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_RTF, "Editor", 							#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_STRING, "String", 						#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_TEXT, "Text", 							#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_SCINTILLA, "Scintilla", 					#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_IPADDRESS, "IP address", 				#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_LINK, "Hyperlink", 						#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_WEB, "Web", 								#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_DATE, "Date", 							#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_CALENDAR, "Calendar", 					#True, ChkNewCommon)
						CloseSubMenu()
					EndIf
					If OpenSubMenuEx ("List", #True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_COMBO, "ListCombo", 						#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_LISTICON, "ListIcon", 					#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_LISTVIEW, "ListView", 					#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_TREE, "TreeView", 						#True, ChkNewCommon)
						MenuBarEx(ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_COMBOEXPLORER, "ExplorerCombo", 			#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_LISTEXPLORER, "ExplorerList", 			#True, ChkNewCommon)
						MenuItemEx (#MENU_TREE_NEW_TREEEXPLORER, "ExplorerTree", 			#True, ChkNewCommon)
						CloseSubMenu()
					EndIf
					MenuBarEx(ChkNewCommon)
					MenuItemEx (#MENU_TREE_NEW_EMPTY, "Empty", 								#True, ChkNewCommon)
					CloseSubMenu()
				EndIf
				MenuBar()
				MenuItemEx(#MENU_TREE_CUT, "Cut" + #TAB$ + "Ctrl+X", 						ChkCut, #True)
				MenuItemEx(#MENU_TREE_COPY, "Copy" + #TAB$ + "Ctrl+C", 						ChkCopy, #True)
				If *CopyObj
					NewText$ = "Paste '" + *CopyObj\pCOMMON\name$ + "'" + #TAB$ + "Ctrl+V"
				Else
					NewText$ = "Paste" + #TAB$ + "Ctrl+V"
				EndIf
				MenuItemEx(#MENU_TREE_PASTE, NewText$, 										ChkPaste, #True)
				MenuBar()
				MenuItemEx(#MENU_TREE_DELETE, "Delete" + #TAB$ + "Del",						ChkDelete, #True)
				MenuItemEx(#MENU_TREE_RENAME, "Rename" + #TAB$ + "F2",						ChkRename, #True)
				MenuBar()
				MenuItemEx(#MENU_TREE_EXPAND, "Expand ++",									ChkExpandCollapse, #True)
				MenuItemEx(#MENU_TREE_COLLAPSE, "Collapse ++",								ChkExpandCollapse, #True)
				MenuBar()
				If OpenSubMenuEx("DD2 Config", 1, 1)
					MenuItemEx(#MENU_TREE__END + 1, "# Code and XML export #", #False)
					MenuItemEx(#MENU_TREE_CFG_DPIPATCH, "Apply DPI-awareness patch",		#True, #True, cfg\DPIPatch)
					MenuItemEx(#MENU_TREE_CFG_NOLOOP, "Don't add event loop",				#True, #True, cfg\NoLoop)
					MenuItemEx(#MENU_TREE_CFG_EXPORT2CB, "Export to clipboard", 			#True, #True, cfg\Export2Clipboard)
					MenuItemEx(#MENU_TREE_CFG_RUNCODE, "Open .pb file after export", 		#True, #True, cfg\RunCode)
					MenuItemEx(#MENU_TREE__END + 2, "# Other #", #False)
					MenuItemEx(#MENU_TREE_CFG_NOCAPTION, "Empty text for new objects", 		#True, #True, cfg\NoCaptions)
					MenuItemEx(#MENU_TREE_CFG_CONFIRMDELETE, "Confirm tree object deletion", 	#True, #True, cfg\ConfirmDelete)
					MenuItemEx(#MENU_TREE_CFG_CONFIRMCHANGES, "Warn about unsaved changes", #True, #True, cfg\ConfirmChanges)
					CloseSubMenu()
				EndIf
				MenuItemEx(#MENU_TREE_REFRESH, "Refresh UI", 	#True, #True)
				MenuBar()
				If cfg\Export2Clipboard
					MenuItemEx(#MENU_TREE_EXPORT_XMLCODE_EX, "Copy Code" + #TAB$ + "Ctrl+E", 	ChkCanExport, #True)
					MenuItemEx(#MENU_TREE_FILE_SAVE, "Copy XML" + #TAB$ + "Ctrl+S", 			ChkCanExport, #True)
				Else
					MenuItemEx(#MENU_TREE_EXPORT_XMLCODE_EX, "Export Code" + #TAB$ + "Ctrl+E", 	ChkCanExport, #True)
					MenuItemEx(#MENU_TREE_FILE_SAVE, "Export XML" + #TAB$ + "Ctrl+S", 			ChkCanExport, #True)
				EndIf
				MenuBar()
				CompilerIf Defined(IS_FULL, #PB_Constant); this is full version (interface tree: load XML menu item)
					MenuItemEx(#MENU_TREE_FILE_LOAD, "Import XML.." + #TAB$ + "Ctrl+O", 		#True, #True)
				CompilerElse
					MenuItemEx(#MENU_TREE_FILE_LOAD, "Import XML.." + #TAB$ + "Ctrl+O", 		#False, #True)
				CompilerEndIf
			;}

			;{ Display menu }

				Protected hwnd = WindowID(WndTree)
				Protected X = -1, Y = -1
				If Not X = -1 And Not Y = -1
					If X < 0
						X = 0
					EndIf
					If Y < 0
						Y = 0
					EndIf
					DisplayPopupMenu(#MENU_TREE__MENU, hwnd, X, Y)
				Else
					DisplayPopupMenu(#MENU_TREE__MENU, hwnd)
				EndIf
			;}

		EndIf
	EndProcedure

;}

;{ UI 	: property wnd }

	Declare WndParamsEditCB()
	Declare WndParamsAfterEditCB()
	Global PARAM_C_HEIGHT = ScaleY(22)			; free space around param control
	Global PARAM_HEIGHT = ScaleY(18)			; param control height
	Global.f PARAM_NAME_WIDTH = 0.33			; % of param list width used for param name control
	Global.f PARAM_VALUE_WIDTH = 0.66			; % used for param value width
	Global PARAM_X_BORDER = ScaleX(4)			; free space around all param controls
	Global PARAM_Y_BORDER = ScaleY(4)			;
	Global PARAM_WND_TIP_HEIGHT = ScaleY(64)	; height of tooltip control
	Global PARAM_WND_OFFSETX = ScaleX(8)		; free space around param window
	Global PARAM_WND_OFFSETY = ScaleY(8)		;
	Global PARAM_WND_WIDTH = ScaleX(240)		; param window width
	Global PARAM_WND_HEIGHT = ScaleY(480)		; param window height
	Global ParamValueFont = LoadFont(#PB_Any, "Arial", 8)
	Global ParamLabelFont = LoadFont(#PB_Any, "Tahoma", 9)
	Global ParamTipFont = ParamLabelFont
	Global WndParams, WndParams_list, WndParams_tip
	Global WNDPARAMS_UPDATETMR = 'pu'	; used to update window with delay, after some param edited
	Global WNDPARAMS_SIZETMR = 'pr'
	Procedure WndParamsSizeTmr()
		Static lW, lH
		Protected cW = WindowWidth(WndParams)
		Protected cH = WindowHeight(WndParams)
		RemoveWindowTimer(WndParams, WNDPARAMS_SIZETMR)
		If Not cw = lw Or Not ch = lh
			If lw And lh
				ResizeGadget(WndParams_list, PARAM_WND_OFFSETX, PARAM_WND_OFFSETY, WindowWidth(WndParams) - PARAM_WND_OFFSETX*2,  WindowHeight(WndParams) - (PARAM_WND_TIP_HEIGHT + PARAM_WND_OFFSETY*2))
				ResizeGadget(WndParams_tip, PARAM_WND_OFFSETX, WindowHeight(WndParams) - (PARAM_WND_TIP_HEIGHT + PARAM_WND_OFFSETY), WindowWidth(WndParams) - PARAM_WND_OFFSETX*2, PARAM_WND_TIP_HEIGHT)
				WndParamsRedraw()
			EndIf
			lw = cW
			lh = cH
		EndIf
	EndProcedure

	;{ Generic param window functions }

		Procedure WndParamsSize()
			AddWindowTimer(WndParams, WNDPARAMS_SIZETMR, 24)
		EndProcedure
		Procedure WndParamsClose()
		EndProcedure
		Procedure WndParamsOpen()
			WndParams = OpenWindow(#PB_Any, 0, 0, PARAM_WND_WIDTH, PARAM_WND_HEIGHT, "Properties", #PB_Window_SizeGadget | #PB_Window_Tool | #PB_Window_Invisible)
			WndParams_list = ScrollAreaGadget(#PB_Any, PARAM_WND_OFFSETX, PARAM_WND_OFFSETY, WindowWidth(WndParams) - PARAM_WND_OFFSETX*2, WindowHeight(WndParams) - (PARAM_WND_TIP_HEIGHT + PARAM_WND_OFFSETY*2), WindowWidth(WndParams) * 0.8, 0, PARAM_C_HEIGHT)
			CloseGadgetList()
			WndParams_tip = TextGadget(#PB_Any, PARAM_WND_OFFSETX, WindowHeight(WndParams) - (PARAM_WND_TIP_HEIGHT + PARAM_WND_OFFSETY), WindowWidth(WndParams) - PARAM_WND_OFFSETX*2, PARAM_WND_TIP_HEIGHT, "")
	  		SetGadgetFont(WndParams_tip, FontID(ParamTipFont))
			WindowBounds(WndParams, PARAM_WND_WIDTH, PARAM_WND_HEIGHT, PARAM_WND_WIDTH*2, #PB_Ignore)
			BindEvent(#PB_Event_CloseWindow, @WndParamsClose(), WndParams)
			BindEvent(#PB_Event_Gadget, @WndParamsEditCB(), WndParams)
			BindEvent(#PB_Event_Timer, @WndParamsAfterEditCB(), WndParams, WNDPARAMS_UPDATETMR)
			BindEvent(#PB_Event_SizeWindow, @WndParamsSize(), WndParams)
			BindEvent(#PB_Event_Timer, @WndParamsSizeTmr(), WndParams, WNDPARAMS_SIZETMR)
			AddWindowTimer(WndParams, WNDPARAMS_SIZETMR, 24)
			CompilerIf Defined(IS_FULL, #PB_Constant)  ; this is full version (interface tree drag&drop files)
				EnableWindowDrop(WndParams, #PB_Drop_Files, #PB_Drag_Copy | #PB_Drag_Move | #PB_Drag_Link)
				BindEvent(#PB_Event_WindowDrop, @DropXMLFile(), WndParams)
			CompilerEndIf
		EndProcedure

	;}

	;{ Global Internal data }

	Structure PARAM_ITEM
		EditorID.i			; handle to a ui element with property value
		LabelID.i			; handle to a label with property name
		*Object.OBJECT		; handle to a object which owns properties
		ParamType.i			; type of data: one of ParamTYPES
		ParamName$			; property name (used to display)
		ParamGroup$			; property name used in splitted params only (i.e. those like flags)
		LeftAlive.a			; cleanup flag
	EndStructure

	Global NewMap ParamEnum.PARAM_ITEM()		; list of actual properties, referenced by property name
	Global NewMap *ParamHandles.PARAM_ITEM()	; list of pointers to ParamEnum(), referenced by EditorID value
	Enumeration ParamTYPES
		#P_BOOLEAN			; boolean values: yes/no
		#P_BOOLEAN_MULTI	; value splitted to several boolean controls
		#P_STRING			; editable string
		#P_STRING_READONLY	; non-editable string field
		#P_INT				; editable number-only
		#P_LIST_FIXED		; list of fixed values
		#P_LIST_STRING		; list of fixed values + editable string
		#P_LIST_INT			; list of fixed values + editable int-only
		#P_BUTTON			; clickable control
		#P_UNUSED			; used for invalid or unsupported types
	EndEnumeration

	Global UWnd
	;}

	; Updates tooltip label of params window
	; RETURN:		none
	Procedure WndParamsSetTIP(Text$)
		SetGadgetText(WndParams_tip, Text$)
	EndProcedure
	; Updates params window title
	; RETURN:		none
	Procedure WndParamsSetCaption(Text$)
		SetWindowTitle(WndParams, "Properties" + Text$)
	EndProcedure

	; Converts param name to corresponding ParamType constant
	; RETURN:		one of #P_ constants
	Procedure GetParamType(ParamName$)
		If Left(ParamName$, 1) = "#"
			ProcedureReturn #P_BOOLEAN_MULTI
		EndIf
		Select ParamName$
			Case #v_invisible, #v_disabled, #v_variable:
				ProcedureReturn #P_BOOLEAN
			Case #v_flags:
				ProcedureReturn #P_BOOLEAN_MULTI
			Case #v_align, #v_expand, #v_colexpand, #v_rowexpand, #v_scrolling:
				ProcedureReturn #P_LIST_FIXED
			Case #v_text, #v_tooltip:
				ProcedureReturn #P_STRING
			Case #v_id, #v_height, #v_width, #v_max, #v_min, #v_value, #v_expandwidth, #v_expandheight, #v_margin,
				#v_columns, #v_rowspan, #v_colspan, #v_spacing, #v_colspacing, #v_rowspacing, #v_group, #v_step, #v_page:
				ProcedureReturn #P_INT
			Case #v_minwidth, #v_maxwidth, #v_minheight, #v_maxheight, #v_innerheight, #v_innerwidth, #v_firstmin, #v_secondmin:
				ProcedureReturn #P_LIST_INT
			Case #v_onevent:
				ProcedureReturn #P_LIST_STRING
			Case #v_name, #v_childs, #v_parent, #v_class:
				ProcedureReturn #P_UNUSED
			Case #v_font, #v_backcolor, #v_forecolor:
				ProcedureReturn #P_BUTTON
			Default:
				Debug "GetParamType: unknown param " + ParamName$
				ProcedureReturn #P_UNUSED
		EndSelect
	EndProcedure

	; Converts param name to string prefix, used then to sort params
	; RETURN:		string
	Procedure$ GetParamSortIndex(ParamName$)
		If Left(ParamName$, 1) = "#"
			ProcedureReturn "1"
		EndIf
		Select ParamName$
			Case #v_name, #v_parent, #v_class, #v_childs:
				ProcedureReturn "0"
			Case #v_id:
				ProcedureReturn "11"
			Case #v_text, #v_tooltip:
				ProcedureReturn "2"
			Case #v_font:
				ProcedureReturn "22"
			Case #v_backcolor, #v_forecolor:
				ProcedureReturn "222"
			Case #v_invisible, #v_disabled:
				ProcedureReturn "3"
			Case #v_onevent, #v_variable:
				ProcedureReturn "33"
			Case #v_innerwidth, #v_minwidth:
				ProcedureReturn "444444"
			Case #v_innerheight, #v_minheight:
				ProcedureReturn "44444"
			Case #v_maxwidth:
				ProcedureReturn "4444"
			Case #v_maxheight:
				ProcedureReturn "444"
			Case #v_width:
				ProcedureReturn "44"
			Case #v_height:
				ProcedureReturn "4"
			Case #v_min:
				ProcedureReturn "55"
			Case #v_max, #v_value:
				ProcedureReturn "5"
			Case #v_firstmin, #v_secondmin, #v_group, #v_step, #v_page, #v_scrolling:
				ProcedureReturn "666"
			Case #v_columns:
				ProcedureReturn "66"
			Case #v_align, #v_expand, #v_margin, #v_colexpand, #v_rowexpand:
				ProcedureReturn "6"
			Case #v_colspacing, #v_rowspacing, #v_spacing:
				ProcedureReturn "777"
			Case #v_expandwidth:
				ProcedureReturn "77"
			Case #v_expandheight:
				ProcedureReturn "7"
			Case #v_rowspan, #v_colspan:
				ProcedureReturn "8"
			Default: 	; unknown/unsupported attribute
				Debug "GetParamSortIndex - unknown param " + ParamName$
				ProcedureReturn "zzz"
		EndSelect
	EndProcedure

	; Those performing auto-update of preview window after user edited property
	Procedure WndParamsPlanUpdate(Delay)
		RemoveWindowTimer(WndParams, WNDPARAMS_UPDATETMR)
		AddWindowTimer(WndParams, WNDPARAMS_UPDATETMR, Delay)
		UWnd = TreeSelectedWindow()
	EndProcedure

	Procedure WndParamsAfterEditCB()
		RemoveWindowTimer(WndParams, WNDPARAMS_UPDATETMR)
		Protected t = WndViewGet()
		If t = UWnd Or t = -1
			WndViewSet(UWnd)
		EndIf
	EndProcedure

	; This callback is called when one of property controls receiving some event
	; All Object editing goes on here
	; RETURN:		#PB_ProcessPureBasicEvents value
	Procedure WndParamsEditCB()
		Protected sHandle$ = Str(EventGadget())
		Protected Value$
		If FindMapElement(*ParamHandles(), sHandle$)
			If EventType() = #PB_EventType_Focus
				WndParamsSetTIP(GetInfo(*ParamHandles()\Object\pEXTRA\type$, *ParamHandles()\ParamName$))
				Protected tC = Random(180, 80)
				SetGadgetColor(*ParamHandles()\LabelID, #PB_Gadget_FrontColor, RGB(tc + Random(24, 0),tc + Random(24, 0),tc + Random(24, 0)));GetSysColor_(#COLOR_HOTLIGHT))
			ElseIf EventType() = #PB_EventType_LostFocus
				WndParamsSetTIP(*ParamHandles()\Object\pCOMMON\name$ + " (" + *ParamHandles()\Object\pEXTRA\Type$ + ")")
				SetGadgetColor(*ParamHandles()\LabelID, #PB_Gadget_FrontColor, #PB_Default)
			EndIf
			Select *ParamHandles()\ParamType
				Case #P_BOOLEAN: 		; CheckBoxGadget
					If EventType() = #PB_EventType_LeftClick
						If GetGadgetState(*ParamHandles()\EditorID)
							Value$ = "yes"
						Else
							Value$ = "no"
						EndIf
						SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
						WndParamsPlanUpdate(1)
					EndIf
				Case #P_BOOLEAN_MULTI: 		; boolean multi item - yes/no
					If EventType() = #PB_EventType_LeftClick
						Value$ = RemoveString(GetValue(*ParamHandles()\Object, *ParamHandles()\ParamGroup$), " ") + "|"
						If GetGadgetState(*ParamHandles()\EditorID)
							If FindString(Value$, *ParamHandles()\ParamName$ + "|", 1, #PB_String_NoCase) = 0
								Value$ + "|" + *ParamHandles()\ParamName$
							EndIf
						Else
							If FindString(Value$, *ParamHandles()\ParamName$ + "|", 1, #PB_String_NoCase)
								Value$ = ReplaceString(Value$, *ParamHandles()\ParamName$ + "|", "", #PB_String_NoCase)
							EndIf
						EndIf
						Value$ = ReplaceString(Value$, "||", "|")
						Value$ = Trim(Value$, "|")
						SetValue(*ParamHandles()\Object, *ParamHandles()\ParamGroup$, Value$)
						WndParamsPlanUpdate(1)
					EndIf
				Case #P_INT: 			; StringGadget with #PB_String_Numeric
					If EventType() = #PB_EventType_Change
						Value$ = Trim(GetGadgetText(*ParamHandles()\EditorID))
						If Value$ = "-"
							Value$ = ""
						EndIf
						If Value$ = "" Or isNumeric(Value$)
							SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
							WndParamsPlanUpdate(1000)
						Else
							SetGadgetText(*ParamHandles()\EditorID, GetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$))
						EndIf
					EndIf
				Case #P_STRING: 		; StringGadget
					If EventType() = #PB_EventType_Change
						Value$ = GetGadgetText(*ParamHandles()\EditorID)
						SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
						WndParamsPlanUpdate(1000)
					EndIf
				Case #P_STRING_READONLY:; StringGadget with #PB_String_ReadOnly
				Case #P_LIST_FIXED: 	; ComboBoxGadget
					If EventType() = #PB_EventType_Change
						Value$ = GetGadgetText(*ParamHandles()\EditorID)
						SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
						WndParamsPlanUpdate(1)
					EndIf
				Case #P_LIST_STRING: 	; ComboBoxGadget with #PB_ComboBox_Editable
					If EventType() = #PB_EventType_Change
						Value$ = GetGadgetText(*ParamHandles()\EditorID)
						If Value$ = "" Or isProc(Value$)
							SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
							WndParamsPlanUpdate(1000)
						Else
							SetGadgetText(*ParamHandles()\EditorID, GetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$))
						EndIf
					EndIf
				Case #P_LIST_INT: 		; ComboBoxGadget with #PB_ComboBox_Editable (numbers-only)
					If EventType() = #PB_EventType_Change
						Value$ = GetGadgetText(*ParamHandles()\EditorID)
						If Value$ = "-"
							Value$ = ""
						EndIf
						If Value$ = "" Or isNumeric(Value$)
							SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
							WndParamsPlanUpdate(1000)
						ElseIf IsFlagSupported(*ParamHandles()\Object\pEXTRA\Type$, *ParamHandles()\ParamName$, Value$)
							SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, Value$)
							WndParamsPlanUpdate(1)
						Else
							SetGadgetText(*ParamHandles()\EditorID, GetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$))
						EndIf
					EndIf
				Case #P_BUTTON:			; selector
					If EventType() = #PB_EventType_LeftClick
						Select *ParamHandles()\ParamName$
							Case #v_font:
								ClearGadgetItems(*ParamHandles()\EditorID)
								If FontRequester(StringField(*ParamHandles()\Object\pCOMMON\font$, 1, "|"), Val(StringField(*ParamHandles()\Object\pCOMMON\font$, 2, "|")), 0, 0, Val(StringField(*ParamHandles()\Object\pCOMMON\font$, 3, "|"))) And SelectedFontName()
									SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, SelectedFontName() + "|" + Str(SelectedFontSize()) + "|" + Str(SelectedFontStyle()))
									WndParamsPlanUpdate(1)
								Else
									SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, "")
								EndIf
								If CountString(*ParamHandles()\Object\pCOMMON\font$, "|") > 1
									AddGadgetItem(*ParamHandles()\EditorID, 0, StringField(*ParamHandles()\Object\pCOMMON\font$, 1, "|") + ", " + StringField(*ParamHandles()\Object\pCOMMON\font$, 2, "|"))
								Else
									AddGadgetItem(*ParamHandles()\EditorID, 0, StringField(*ParamHandles()\Object\pCOMMON\font$, 1, "|"))
								EndIf
							Case #v_forecolor, #v_backcolor:
								SetGadgetState(*ParamHandles()\EditorID, -1)
								Value$ = Hex(ColorRequester(Val(GetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$))), #PB_Long)
								If Not Value$ = "FFFFFFFF"
									SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, "$" + Value$)
									SetGadgetColor(*ParamHandles()\EditorID, #PB_Gadget_BackColor, Val("$" + Value$))
								Else
									SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, "")
									SetGadgetColor(*ParamHandles()\EditorID, #PB_Gadget_BackColor, #PB_Default)
								EndIf
								WndParamsPlanUpdate(1)
						EndSelect
					ElseIf EventType() = #PB_EventType_RightClick
						SetValue(*ParamHandles()\Object, *ParamHandles()\ParamName$, "")
						ClearGadgetItems(*ParamHandles()\EditorID)
						AddGadgetItem(*ParamHandles()\EditorID, 0, "")
						SetGadgetColor(*ParamHandles()\EditorID, #PB_Gadget_BackColor, #PB_Default)
						WndParamsPlanUpdate(1)
					EndIf
				Case #P_UNUSED:			; ??
			EndSelect
		EndIf
		ProcedureReturn #PB_ProcessPureBasicEvents
	EndProcedure

	; Reloads whole properties list control
	; *obj			object to show
	; RETURN:		none
	Procedure WndParamsList (*obj.OBJECT)
		Protected objParent$			; parent treename
		Protected objParentName$		; parent object name
		Protected objParentType$		; parent class
		Protected objType$				; obj type
		Protected objName$				; obj name
		If *obj
			objParent$ = Tree(*obj\treeName$)\Parent$
			objType$ = *obj\pEXTRA\type$
			objName$ = *obj\pCOMMON\name$
		EndIf
		If objParent$
			objParentType$ = Tree(objParent$)\Data\pEXTRA\Type$
			objParentName$ = Tree(objParent$)\Data\pCOMMON\name$
		EndIf
		Macro AddAttribute(name, value, group = "")
			WndParamsSet (*obj, name, value, group)
		EndMacro

		Protected NewList Flags$()
		Protected TFlags$
		Macro AddFlags (name, value)
			Tflags$ = RemoveString(value, " ") + "|"
			GetObjectFlagsList(objType$, name, Flags$())
			ForEach Flags$()
				If FindString(Tflags$, Flags$() + "|", 1, #PB_String_NoCase)
					AddAttribute(Flags$(), "yes", name)
				Else
					AddAttribute(Flags$(), "no", name)
				EndIf
			Next
		EndMacro

		If *obj
			AddAttribute(#v_class, 		objType$)
			AddAttribute(#v_name, 		objName$)
			AddAttribute(#v_childs, 	Str(TreeGetChildCount(Tree(), *obj\treeName$)))
			AddAttribute(#v_parent, 	objParentName$)
			Protected NewList ObjAttrList$()
			GetSupportedValuesList(*obj\pEXTRA\Type$, ObjAttrList$())
			ForEach ObjAttrList$()
				Select ObjAttrList$()
					Case #v_flags:
						AddFlags (#v_flags, GetValue(*obj, ObjAttrList$()))
					Case #v_name: ;, #v_id:
					Case #v_colspan, #v_rowspan:
						If objParentType$ = #C_GRIDBOX
							AddAttribute(ObjAttrList$(), GetValue(*obj, ObjAttrList$()))
						EndIf
					Case "":
						Break
					Default
						AddAttribute(ObjAttrList$(), GetValue(*obj, ObjAttrList$()))
				EndSelect
			Next
		EndIf
		UndefineMacro AddAttribute
		UndefineMacro AddFlags
		ForEach ParamEnum()
			If ParamEnum()\LeftAlive = #False
				FreeGadget(ParamEnum()\EditorID)
				FreeGadget(ParamEnum()\LabelID)
				DeleteMapElement(*ParamHandles(), Str(ParamEnum()\EditorID))
				DeleteMapElement(ParamEnum())
			Else
				ParamEnum()\LeftAlive = #False
			EndIf
		Next
		If *obj
			WndParamsSetTIP(objName$ + " (" + objType$ + ")")
			WndParamsSetCaption(": " + objName$)
		Else
			WndParamsSetTIP("")
			WndParamsSetCaption("")
		EndIf
		WndParamsRedraw()
	EndProcedure

	; Adds single item to a current properties list (or updates existing if it already exists)
	; It also marks element with LeftAlive, so element will not be deleted on next scroll list update
	; RETURN:		none
	Procedure WndParamsSet(*Object.OBJECT, Param$, Value$, Group$ = "")
		Protected ParamType = GetParamType(Param$)
		Protected ItemOK = #True
		If FindMapElement(ParamEnum(), Param$) And Not ParamEnum(Param$)\ParamType = ParamType
			FreeGadget(ParamEnum()\EditorID)
			FreeGadget(ParamEnum()\LabelID)
			DeleteMapElement(*ParamHandles(), Str(ParamEnum()\EditorID))
			DeleteMapElement(ParamEnum())
		EndIf
		If FindMapElement(ParamEnum(), Param$) = 0
			OpenGadgetList(WndParams_list)
			Select ParamType
				Case #P_BOOLEAN: 		; boolean items
					ParamEnum(Param$)\EditorID = CheckBoxGadget(#PB_Any, 0, 0, 1, 1, "")
				Case #P_BOOLEAN_MULTI:
					ParamEnum(Param$)\EditorID = CheckBoxGadget(#PB_Any, 0, 0, 1, 1, "")
					ParamEnum(Param$)\ParamGroup$ = Group$
				Case #P_INT: 			; editable number-only
					ParamEnum(Param$)\EditorID = StringGadget(#PB_Any, 0, 0, 1, 1, "")
				Case #P_STRING: 		; editable string
					ParamEnum(Param$)\EditorID = StringGadget(#PB_Any, 0, 0, 1, 1, "")
				Case #P_STRING_READONLY:; non-editable string
					ParamEnum(Param$)\EditorID = StringGadget(#PB_Any, 0, 0, 1, 1, "", #PB_String_ReadOnly)
				Case #P_LIST_FIXED: 	; fixed list
					ParamEnum(Param$)\EditorID = ComboBoxGadget(#PB_Any, 0, 0, 1, 1)
				Case #P_LIST_STRING: 	; list + editable string
					ParamEnum(Param$)\EditorID = ComboBoxGadget(#PB_Any, 0, 0, 1, 1, #PB_ComboBox_Editable)
				Case #P_LIST_INT: 		; list + editable integer
					ParamEnum(Param$)\EditorID = ComboBoxGadget(#PB_Any, 0, 0, 1, 1, #PB_ComboBox_Editable)
				Case #P_BUTTON:			; selector
					ParamEnum(Param$)\EditorID = ListViewGadget(#PB_Any, 0, 0, 1, 1)
				Case #P_UNUSED:			; ??
					ParamEnum(Param$)\EditorID = StringGadget(#PB_Any, 0, 0, 1, 1, "", #PB_String_ReadOnly)
				Default:
					ItemOK = #False
			EndSelect
			If ItemOK
				If ParamType = #P_BOOLEAN_MULTI
					ParamEnum(Param$)\LabelID = TextGadget(#PB_Any, 0, 0, 1, 1, Group$ + ":")
				ElseIf ParamType = #P_STRING_READONLY Or ParamType = #P_UNUSED
					ParamEnum(Param$)\LabelID = TextGadget(#PB_Any, 0, 0, 1, 1, "! " + Param$)
				Else
					ParamEnum(Param$)\LabelID = TextGadget(#PB_Any, 0, 0, 1, 1, Param$)
				EndIf
				SetGadgetFont(ParamEnum(Param$)\LabelID, FontID(ParamLabelFont))
				SetGadgetFont(ParamEnum(Param$)\EditorID, FontID(ParamValueFont))
				*ParamHandles(Str(ParamEnum(Param$)\EditorID)) = @ParamEnum(Param$)
			EndIf
			CloseGadgetList()
		EndIf
		Protected NewList FlagList$()
		If ItemOK
		Select ParamType
			Case #P_BOOLEAN: 			; boolean items - yes/no
				SetGadgetState(ParamEnum(Param$)\EditorID, Bool(LCase(Value$) = "yes"))
				GadgetToolTip(ParamEnum(Param$)\EditorID, GetInfo(*Object\pEXTRA\type$, Param$))
			Case #P_BOOLEAN_MULTI: 		; boolean multi item - yes/no
				SetGadgetState(ParamEnum(Param$)\EditorID, Bool(LCase(Value$) = "yes"))
				SetGadgetText(ParamEnum(Param$)\EditorID, Param$)
				GadgetToolTip(ParamEnum(Param$)\EditorID, Param$)
			Case #P_INT: 			; editable number-only
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
			Case #P_STRING: 		; editable string
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
			Case #P_STRING_READONLY:; non-editable string
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
			Case #P_LIST_FIXED: 	; fixed list
				GetObjectFlagsList(*Object\pEXTRA\Type$, Param$, FlagList$())
				AddElement(FlagList$())
				MoveElement(FlagList$(), #PB_List_First)
				Protected tCc, tCm
				Select *Object\pEXTRA\Type$
					Case #C_HBOX, #C_VBOX:
						Select Param$
							Case #v_expand:
								tCm = TreeGetChildCount(Tree(), *Object\treeName$)
								For tCc = 1 To tCm
									AddElement(FlagList$())
									FlagList$() = "item:" + Str(tCc)
								Next
						EndSelect
					Case #C_GRIDBOX:
						Select Param$
							Case #v_colexpand:
								tCm = Val(*Object\pEXTRA\gridbox\columns$)
								For tCc = 1 To tCm
									AddElement(FlagList$())
									FlagList$() = "item:" + Str(tCc)
								Next
							Case #v_rowexpand:
								tCm = TreeGetChildCount(Tree(), *Object\treeName$)
								For tCc = 1 To tCm
									AddElement(FlagList$())
									FlagList$() = "item:" + Str(tCc)
								Next
						EndSelect
				EndSelect
				ClearGadgetItems(ParamEnum(Param$)\EditorID)
				ForEach FlagList$()
					AddGadgetItem(ParamEnum(Param$)\EditorID, -1, FlagList$())
				Next
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
			Case #P_LIST_STRING: 	; list + editable string
				GetObjectFlagsList(*Object\pEXTRA\Type$, Param$, FlagList$())
				AddElement(FlagList$())
				MoveElement(FlagList$(), #PB_List_First)
				ClearGadgetItems(ParamEnum(Param$)\EditorID)
				ForEach FlagList$()
					AddGadgetItem(ParamEnum(Param$)\EditorID, -1, FlagList$())
				Next
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
			Case #P_LIST_INT: 		; list + editable integer
				GetObjectFlagsList(*Object\pEXTRA\Type$, Param$, FlagList$())
				AddElement(FlagList$())
				MoveElement(FlagList$(), #PB_List_First)
				ClearGadgetItems(ParamEnum(Param$)\EditorID)
				ForEach FlagList$()
					AddGadgetItem(ParamEnum(Param$)\EditorID, -1, FlagList$())
				Next
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
			Case #P_BUTTON:			; selector
				ClearGadgetItems(ParamEnum(Param$)\EditorID)
				GadgetToolTip(ParamEnum(Param$)\EditorID, GetInfo(*Object\pEXTRA\Type$, Param$))
				Select Param$
					Case #v_font:
						If CountString(Value$, "|") > 1
							AddGadgetItem(ParamEnum(Param$)\EditorID, 0, StringField(Value$, 1, "|") + ", " + StringField(Value$, 2, "|"))
						Else
							AddGadgetItem(ParamEnum(Param$)\EditorID, 0, StringField(Value$, 1, "|"))
						EndIf
					Case #v_backcolor, #v_forecolor:
						AddGadgetItem(ParamEnum(Param$)\EditorID, 0, "")
						If Value$
							SetGadgetColor(ParamEnum(Param$)\EditorID, #PB_Gadget_BackColor, Val(Value$))
						Else
							SetGadgetColor(ParamEnum(Param$)\EditorID, #PB_Gadget_BackColor, #PB_Default)
						EndIf
				EndSelect
			Case #P_UNUSED:			; ??
				SetGadgetText(ParamEnum(Param$)\EditorID, Value$)
		EndSelect
		EndIf
		If ItemOK
			ParamEnum(Param$)\Object = *Object
			ParamEnum(Param$)\LeftAlive = #True
			ParamEnum(Param$)\ParamName$ = Param$
			ParamEnum(Param$)\ParamType = ParamType
		EndIf
	EndProcedure

	; Performs smart update and redraw of scroll area. All items having no LeftAlive = #true are deleted here
	; Should be called after adding/updating all properties.
	; RETURN:		none
	Procedure WndParamsRedraw()
		SetGadgetAttribute(WndParams_list, #PB_ScrollArea_InnerHeight, PARAM_Y_BORDER + MapSize(ParamEnum()) * PARAM_C_HEIGHT)
		SetGadgetAttribute(WndParams_list, #PB_ScrollArea_InnerWidth, GadgetWidth(WndParams_list) - (PARAM_X_BORDER + GetSystemMetrics_(#SM_CXVSCROLL)) )
		Protected iW = GetGadgetAttribute(WndParams_list, #PB_ScrollArea_InnerWidth)
		Protected iWName = iW * PARAM_NAME_WIDTH - PARAM_X_BORDER
		Protected iWData = iW * PARAM_VALUE_WIDTH
		Protected T
		Protected cX, Cy, cW, cH
		Structure SORTEDITEM
			SortField$
			*item.PARAM_ITEM
		EndStructure
		Protected NewList SortedEnum.SORTEDITEM()
		ForEach ParamEnum()
			AddElement(SortedEnum())
			SortedEnum()\item = @ParamEnum()
			SortedEnum()\SortField$ = GetParamSortIndex (SortedEnum()\item\ParamName$) + "_" + SortedEnum()\item\ParamName$
		Next
		SortStructuredList(SortedEnum(), #PB_Sort_Ascending | #PB_Sort_NoCase, OffsetOf(SORTEDITEM\SortField$), #PB_String)
		Protected *Current.PARAM_ITEM
		ForEach SortedEnum()
			*Current = SortedEnum()\item
			Protected pWnd
			SetWindowPos_(GadgetID(*Current\EditorID), pWnd, 0, 0, 0, 0, #SWP_NOMOVE|#SWP_NOSIZE);
			pWnd = GadgetID(*Current\EditorID)
			cX = PARAM_X_BORDER
			cY = 2 + PARAM_Y_BORDER + T * PARAM_C_HEIGHT
			cW = iWName
			cH = PARAM_HEIGHT
			If Not GadgetX(*Current\LabelID) = cX Or Not GadgetY(*Current\LabelID) = cY Or Not GadgetWidth(*Current\LabelID) = cW Or Not GadgetHeight(*Current\LabelID) = cH
				ResizeGadget(*Current\LabelID, cX, cY, cW, cH)
			EndIf
			cX = iWName + PARAM_X_BORDER
			cY = PARAM_Y_BORDER + T * PARAM_C_HEIGHT
			cW = iWData
			cH = PARAM_HEIGHT
			If Not GadgetX(*Current\EditorID) = cX Or Not GadgetY(*Current\EditorID) = cY Or Not GadgetWidth(*Current\EditorID) = cW Or Not GadgetHeight(*Current\EditorID) = cH
				ResizeGadget(*Current\EditorID, cX, cY, cW, cH)
			EndIf
			T + 1
		Next
		FreeList(SortedEnum())
	EndProcedure

;}

;{ UI 	: preview wnd }

	Global LastDialog
	Global WndLastDialog
	Global *LastWindow.OBJECT
	Global WNDVIEW_W = ScaleX(800)
	Global WNDVIEW_H = ScaleY(600)
	Global WNDVIEW_DIALOGX = ScaleX(8)
	Global WNDVIEW_DIALOGY = ScaleY(8)
	Global WNDVIEW_SIZETMR = 'vr'
	Structure PREVIEWFONT
		FontName$
		FontSize.l
		FontStyle.l
		Font.i
		Used.a
	EndStructure
	Structure PREVIEWOBJECT
		*FontData.PREVIEWFONT	; pointer to PreviewFonts() element, or 0
		ColorFore.l
		ColorBack.l
		useFore.a				; true to set fore color
		useBack.a				; true to set back
		Tip$					; text tooltip
	EndStructure
	Global NewMap PreviewFonts.PREVIEWFONT()			; contains data about fonts used in preview
	Global NewMap PreviewObjects.PREVIEWOBJECT()		; data about controls of preview dialog
	Procedure WndViewSizeTmr()
		Static lW, lH
		Protected cW = WindowWidth(WndView)
		Protected cH = WindowHeight(WndView)
		RemoveWindowTimer(WndView, WNDVIEW_SIZETMR)
		If Not cw = lw Or Not ch = lh
			If lw And lh
				Protected cX, cY
				cX = (WindowX(WndTree, #PB_Window_FrameCoordinate) - WindowX(WndView, #PB_Window_InnerCoordinate)) + (cw - lw)
				cY = (WindowY(WndTree, #PB_Window_FrameCoordinate) - WindowY(WndView, #PB_Window_InnerCoordinate))
				If cW - cX < 32
					cX = cW - 32
				EndIf
				If cH - cY < 32
					cY = cH - 32
				EndIf
				If cX < 0
					cX = 0
				EndIf
				If cY < 0
					cY = 0
				EndIf
				ResizeWindow(WndTree, cX, cY, #PB_Ignore, #PB_Ignore)
				cX = (WindowX(WndParams, #PB_Window_FrameCoordinate) - WindowX(WndView, #PB_Window_InnerCoordinate)) + (cw - lw)
				cY = (WindowY(WndParams, #PB_Window_FrameCoordinate) - WindowY(WndView, #PB_Window_InnerCoordinate)) + (ch - lh)
				If cW - cX < 32
					cX = cW - 32
				EndIf
				If cH - cY < 32
					cY = cH - 32
				EndIf
				If cX < 0
					cX = 0
				EndIf
				If cY < 0
					cY = 0
				EndIf
				ResizeWindow(WndParams, cX, cY, #PB_Ignore, #PB_Ignore)
			EndIf
			lw = cW
			lh = cH
		EndIf
		ProcedureReturn 0
	EndProcedure

	Procedure WndViewSize()
		AddWindowTimer(WndView, WNDVIEW_SIZETMR, 24)
	EndProcedure

	Procedure WndViewOpen()
		WndView = OpenWindow(#PB_Any, 0, 0, WNDVIEW_W, WNDVIEW_H, APP_TITLE$ + APP_TITLE_EX$, #PB_Window_SystemMenu | #PB_Window_SizeGadget | #PB_Window_ScreenCentered | #PB_Window_MaximizeGadget | #PB_Window_MinimizeGadget)
		WindowBounds(WndView, WNDVIEW_W, WNDVIEW_H, #PB_Ignore, #PB_Ignore)
		BindEvent(#PB_Event_SizeWindow, @WndViewSize(), WndView)
		BindEvent(#PB_Event_Timer, @WndViewSizeTmr(), WndView, WNDVIEW_SIZETMR)
		AddWindowTimer(WndView, WNDVIEW_SIZETMR, 24)
		CompilerIf Defined(IS_FULL, #PB_Constant)  ; this is full version (interface tree drag&drop files)
			EnableWindowDrop(WndView, #PB_Drop_Files, #PB_Drag_Copy | #PB_Drag_Move | #PB_Drag_Link)
			BindEvent(#PB_Event_WindowDrop, @DropXMLFile(), WndView)
		CompilerEndIf
	EndProcedure

	; set title of main (preview) window
	Procedure WndViewTitle(Text$)
		If Text$
			SetWindowTitle(WndView, APP_TITLE$ + APP_TITLE_EX$ + ": " + Text$)
		Else
			SetWindowTitle(WndView, APP_TITLE$ + APP_TITLE_EX$)
		EndIf
	EndProcedure

	; This processes messages from preview dialog window itself
	Procedure WndViewDialogMsg (hWnd, uMsg, WParam, LParam)
		Select uMsg
			Case #WM_MOVING:
				Protected *T.RECT = LParam
				Protected XDiff = *T\left - (WindowX(WndView, #PB_Window_InnerCoordinate) + WNDVIEW_DIALOGX)
				Protected YDiff = *T\top - (WindowY(WndView, #PB_Window_InnerCoordinate) + WNDVIEW_DIALOGY)
				*T\left - XDiff
				*T\top - YDiff
				*T\right - XDiff
				*T\bottom - YDiff
			Case #WM_SIZING:
				*T.RECT = LParam
				Select WParam
					Case #WMSZ_TOP, #WMSZ_TOPRIGHT:
						*T\top = (WindowY(WndView, #PB_Window_InnerCoordinate) + WNDVIEW_DIALOGY)
					Case #WMSZ_LEFT, #WMSZ_BOTTOMLEFT:
						*T\left = (WindowX(WndView, #PB_Window_InnerCoordinate) + WNDVIEW_DIALOGX)
					Case #WMSZ_TOPLEFT:
						*T\left = (WindowX(WndView, #PB_Window_InnerCoordinate) + WNDVIEW_DIALOGX)
						*T\top = (WindowY(WndView, #PB_Window_InnerCoordinate) + WNDVIEW_DIALOGY)
				EndSelect
			Case #WM_ACTIVATE:
				SetWindowPos_(hWnd, #HWND_BOTTOM, 0, 0, 0, 0, #SWP_NOMOVE | #SWP_NOSIZE | #SWP_NOACTIVATE)
			Case #WM_SYSCOMMAND:
				If WParam = $F012
					ProcedureReturn 0
				EndIf
			Default:
		EndSelect
		;
		ProcedureReturn #PB_ProcessPureBasicEvents
	EndProcedure

	; Register font to use in preview dialog
	; RETURN:		none
	Procedure WndViewAddFont(ObjName$, Name$, Size, Style)
		Protected FontKey$ = Name$ + "|" + Str(SIZE) + "|" + Str(Style)
		If FindMapElement(PreviewFonts(), FontKey$)
		Else
			PreviewFonts(FontKey$)\FontName$ = Name$
			PreviewFonts(FontKey$)\FontSize = Size
			PreviewFonts(FontKey$)\FontStyle = Style
		EndIf
		PreviewFonts(FontKey$)\Used = #True
		PreviewObjects(ObjName$)\FontData = @PreviewFonts()
	EndProcedure

	; Register color for preview
	; RETURN:		none
	Procedure WndViewAddColor(ObjName$, Color, Type = #PB_Gadget_FrontColor)
		If Type = #PB_Gadget_FrontColor
			PreviewObjects(ObjName$)\ColorFore = Color
			PreviewObjects(ObjName$)\useFore = #True
		Else
			PreviewObjects(ObjName$)\ColorBack = Color
			PreviewObjects(ObjName$)\useBack = #True
		EndIf
	EndProcedure

	; Register tooltip text
	; RETURN:		none
	Procedure WndViewAddTip(ObjName$, Tip$)
		PreviewObjects(ObjName$)\Tip$ = Tip$
	EndProcedure

	; draws preview dialog window
	; *CWindow		an object of type #C_WINDOW to draw
	; RETURN:		none
	Procedure WndViewSet(*CWindow.OBJECT)
		WndViewTitle("")
		If IsDialog(LastDialog)
			FreeDialog(LastDialog)
			*LastWindow = 0
		EndIf
		If IsWindow(WndLastDialog)
			SetWindowCallback(0, WndLastDialog)
			CloseWindow(WndLastDialog)
		EndIf
		ForEach PreviewFonts()
			PreviewFonts()\Used = #False
		Next
		ClearMap(PreviewObjects())
		If Not *CWindow
			ProcedureReturn
		EndIf
		Protected.q Speed = ElapsedMilliseconds()
		Protected LastAppWnd = GetActiveWindow()
		If GenerateXML(#True, #False, #False, *CWindow\pCOMMON\name$) = #False
			ProcedureReturn
		EndIf
		If IsXML(XML_current)
			If XMLStatus(XML_current) = #PB_XML_Success
				LastDialog = CreateDialog(#PB_Any)
				SetGadgetFont(#PB_Default, #PB_Default)
				If FindMapElement(PreviewObjects(), #C_DIALOGS)
					If PreviewObjects()\FontData
						If IsFont(PreviewObjects()\FontData\Font) = #False
							PreviewObjects()\FontData\Font = LoadFont(#PB_Any, PreviewObjects()\FontData\FontName$, PreviewObjects()\FontData\FontSize, PreviewObjects()\FontData\FontStyle)
						EndIf
						If IsFont(PreviewObjects()\FontData\Font)
							SetGadgetFont(#PB_Default, FontID(PreviewObjects()\FontData\Font))
						EndIf
					EndIf
				EndIf
				If OpenXMLDialog(LastDialog, XML_current, *CWindow\pCOMMON\name$)
					*LastWindow = *CWindow
					WndViewTitle("'" + *CWindow\pCOMMON\name$ + "'")
					WndLastDialog = DialogWindow(LastDialog)
					If IsWindow(WndLastDialog)
						SetParent_(WindowID(WndLastDialog), WindowID(WndView))
						Protected TWnd
						ForEach PreviewObjects()
							TWnd = DialogGadget(LastDialog, MapKey(PreviewObjects()))
							If IsGadget(TWnd)
								If PreviewObjects()\FontData
									If IsFont(PreviewObjects()\FontData\Font) = #False
										PreviewObjects()\FontData\Font = LoadFont(#PB_Any, PreviewObjects()\FontData\FontName$, PreviewObjects()\FontData\FontSize, PreviewObjects()\FontData\FontStyle)
									EndIf
									If IsFont(PreviewObjects()\FontData\Font)
										SetGadgetFont(TWnd, FontID(PreviewObjects()\FontData\Font))
									EndIf
								EndIf
								If PreviewObjects()\useFore
									SetGadgetColor(TWnd, #PB_Gadget_FrontColor, PreviewObjects()\ColorFore)
								EndIf
								If PreviewObjects()\useBack
									SetGadgetColor(TWnd, #PB_Gadget_BackColor, PreviewObjects()\ColorBack)
								EndIf
								If PreviewObjects()\Tip$
									GadgetToolTip(TWnd, PreviewObjects()\Tip$)
								EndIf
							EndIf
						Next
						ForEach PreviewFonts()
							If PreviewFonts()\Used = #False
								If IsFont(PreviewFonts()\Font)
									FreeFont(PreviewFonts()\Font)
								EndIf
								DeleteMapElement(PreviewFonts())
							EndIf
						Next
						RefreshDialog(LastDialog)
						ResizeWindow(WndLastDialog, WNDVIEW_DIALOGX, WNDVIEW_DIALOGY, #PB_Ignore, #PB_Ignore)
						SetWindowPos_(WindowID(WndLastDialog), #HWND_BOTTOM, 0, 0, 0, 0, #SWP_NOMOVE | #SWP_NOSIZE | #SWP_NOACTIVATE | #SWP_SHOWWINDOW)
						SetWindowCallback(@WndViewDialogMsg(), WndLastDialog)
					EndIf
					If IsWindow(LastAppWnd)
						SetActiveWindow(LastAppWnd)
					EndIf
				Else
					SetActiveWindow(WndView)
					WndViewTitle("(error: " + DialogError(LastDialog) + ")")
					FreeDialog(LastDialog)
				EndIf
			Else
				SetActiveWindow(WndView)
				WndViewTitle("(XML error: " + XMLErrorLine(XML_current) + ":: " + XMLError(XML_current) + ")")
			EndIf
			If Not *LastWindow = *CWindow
				*LastWindow = -1
			EndIf
		Else
			WndViewTitle("(error: cannot generate XML)")
			SetActiveWindow(WndView)
		EndIf
		Debug "[delay] WndViewSet: " + Str(ElapsedMilliseconds() - Speed)
	EndProcedure

	; returns handle to a window object which was used in WndViewSet() last time
	Procedure WndViewGet()
		ProcedureReturn *LastWindow
	EndProcedure

;}

ReadCfg(cfg, cfgPath$)
WndViewOpen()
WndTreeOpen()
WndParamsOpen()
SetParent_(WindowID(WndTree), WindowID(WndView))
SetParent_(WindowID(WndParams), WindowID(WndView))
SetWindowPos_(WindowID(WndTree), #HWND_TOPMOST, 0, 0, 0, 0, #SWP_NOMOVE | #SWP_NOSIZE | #SWP_NOACTIVATE)
SetWindowPos_(WindowID(WndParams), #HWND_TOPMOST, 0, 0, 0, 0, #SWP_NOMOVE | #SWP_NOSIZE | #SWP_NOACTIVATE)
ResizeWindow(WndTree, WindowWidth(WndView) - WindowWidth(WndTree, #PB_Window_FrameCoordinate), 0, #PB_Ignore, #PB_Ignore)
ResizeWindow(WndParams, WindowWidth(WndView) - (WindowWidth(WndTree, #PB_Window_FrameCoordinate) + WindowWidth(WndParams, #PB_Window_FrameCoordinate) * 0.5), WindowHeight(WndView) - WindowHeight(WndParams, #PB_Window_FrameCoordinate), #PB_Ignore, #PB_Ignore)
HideWindow(WndTree, #False, #PB_Window_NoActivate)
HideWindow(WndParams, #False, #PB_Window_NoActivate)
SetActiveWindow(WndTree)
objNew("", #C_DIALOGS, #C_DIALOGS)
XMLSetState(XMLHash())
CompilerIf #PB_Compiler_Debugger
	LoadXMLFile("sandbox.xml")
CompilerEndIf
Define NewMap ExpandedOnStart()
ExpandedOnStart("WhoWantsToLiveForever") = #True
RedrawTree(ExpandedOnStart(), #C_DIALOGS)
RedrawWindow_(GadgetID(WndTree_list), 0, 0, #RDW_ERASE)
FreeMap(ExpandedOnStart())
WndParamsList(TreeSelectedObject())
Repeat
	Define E = WaitWindowEvent()
	If EventWindow() = WndView And E = #PB_Event_CloseWindow
		If cfg\ConfirmChanges And Not (XMLHash() = XMLGetState())
			If MessageRequester(APP_TITLE$, ~"Quit without saving XML?", #PB_MessageRequester_Info | #PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
				Break
			EndIf
		Else
			Break
		EndIf
	EndIf
ForEver
SetWindowCallback(0)
FreeFont(#PB_All)
CloseWindow(#PB_All)
SaveCfg(cfg, cfgPath$, #True)
End