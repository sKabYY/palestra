(function (g) {
    /* === */
    var ReactElement = function (type, key, props) {
        this.type = type;
        this.key = key;
        this.props = props;
    };
    var ReactClass = function () {};
    ReactClass.prototype.render = function () {};
    ReactClass.prototype.setState = function (newState) {
        this._reactInternalInstance.receiveComponent(null, newState);
    };
    /* === */

    /* === */
    var ReactDOMTextComponent = function (text) {
        this._currentElement = '' + text;
        this._rootNodeId = null;
    };

    ReactDOMTextComponent.prototype.mountComponent = function (rootId) {
        this._rootNodeId = rootId;
        this._renderedComponent = this;
        return '<span data-reactid="' + rootId + '">' + this._currentElement + '</span>';
    };

    ReactDOMTextComponent.prototype.receiveComponent = function (nextText) {
        var nextStringText = '' + nextText;
        if (nextStringText !== this._currentElement) {
            this._currentElement = nextStringText;
            $('[data-reactid="' + this._rootNodeId + '"]').html(nextStringText);
        }
    };

    var ReactDOMComponent = function (element) {
        this._currentElement = element;
        this._rootNodeId = null;
    };

    ReactDOMComponent.prototype.mountComponent = function (rootId) {
        this._rootNodeId = rootId;
        var props = this._currentElement.props;
        var tagOpen = '<' + this._currentElement.type;
        var tagClose = '</' + this._currentElement.type + '>';
        tagOpen += ' data-reactid="' + rootId + '"';
        for (var propKey in props) {
            if (/^on[A-Za-z]/.test(propKey)) {
                var eventType = propKey.replace('on', '');
                $(document).delegate('[data-reactid="' + rootId + '"]', eventType + '.'  + rootId, props[propKey]);
            }
            if (props[propKey] && propKey != 'children' && !/^on[A-Za-z]/.test(propKey)) {
                tagOpen += ' ' + propKey + '=' + props[propKey];
            }
        }
        var content = '';
        var children = props.children || [];
        var childrenInstances = [];
        $.each(children, function (key, child) {
            var childComponentInstance = instantiateReactComponent(child);
            childComponentInstance._mountIndex = key;
            childrenInstances.push(childComponentInstance);
            var curRootId = rootId + '.' + key;
            var childMarkup = childComponentInstance.mountComponent(curRootId);
            content += ' ' + childMarkup;
        });
        this._renderedChildren = childrenInstances;
        this._renderedComponent = this;
        return tagOpen + '>' + content + tagClose;
    };

    ReactDOMComponent.prototype.receiveComponent = function (nextElement) {
        var lastProps = this._currentElement.props;
        var nextProps = nextElement.props;
        this._currentElement = nextElement;
        this._updateDOMProperties(lastProps, nextProps);
        this._updateDOMChildren(nextElement.props.children);
    };

    ReactDOMComponent.prototype._updateDOMProperties = function (lastProps, nextProps) {
        var propKey;
        for (propKey in lastProps) {
            if (nextProps.hasOwnProperty(propKey) || ! lastProps.hasOwnProperty(propKey)) {
                continue;
            }
            if (/^on[A-Z-a-z]/.test(propKey)) {
                var eventType = propKey.replace('on', '');
                $(document).undelegate('[data-reactid="' + this._rootNodeId + '"]', eventType, lastProps[propKey]);
                continue;
            }
            $('[data-reactid="' + this._rootNodeId + '"]').removeAttribute(propKey);
        }
        for (propKey in nextProps) {
            if (/^on[A-Za-z]/.test(propKey)) {
                var eventType = propKey.replace('on', '');
                lastProps[propKey] && $(document).undelegate('[data-reactid="' + this._rootNodeId + '"]', eventType, lastProps[propKey]);
                continue;
            }
            if (propKey == 'children') continue;
            $('[data-reactid="' + this._rootNodeId + '"]').prop(propKey, nextProps[propKey]);
        }
    };

    //全局的更新深度标识
    var updateDepth = 0;
    //全局的更新队列，所有的差异都存在这里
    var diffQueue = [];


    ReactDOMComponent.prototype._updateDOMChildren = function (nextChildrenElements){
        updateDepth++;
        //_diff用来递归找出差别,组装差异对象,添加到更新队列diffQueue。
        this._diff(diffQueue,nextChildrenElements);
        updateDepth--;
        if(updateDepth == 0){
            //在需要的时候调用patch，执行具体的dom操作
            this._patch(diffQueue);
            diffQueue = [];
        }
    };
    //差异更新的几种类型
    var UPATE_TYPES = {
        MOVE_EXISTING: 1,
        REMOVE_NODE: 2,
        INSERT_MARKUP: 3
    }


//普通的children是一个数组，此方法把它转换成一个map,key就是element的key,如果是text节点或者element创建时并没有传入key,就直接用在数组里的index标识
    function flattenChildren(componentChildren) {
        var child;
        var name;
        var childrenMap = {};
        for (var i = 0; i < componentChildren.length; i++) {
            child = componentChildren[i];
            name = child && child._currentelement && child._currentelement.key ? child._currentelement.key : i.toString(36);
            childrenMap[name] = child;
        }
        return childrenMap;
    }


//主要用来生成子节点elements的component集合
//这边注意，有个判断逻辑，如果发现是更新，就会继续使用以前的componentInstance,调用对应的receiveComponent。
//如果是新的节点，就会重新生成一个新的componentInstance，
    function generateComponentChildren(prevChildren, nextChildrenElements) {
        var nextChildren = {};
        nextChildrenElements = nextChildrenElements || [];
        $.each(nextChildrenElements, function(index, element) {
            var name = element.key ? element.key : index;
            var prevChild = prevChildren && prevChildren[name];
            var prevElement = prevChild && prevChild._currentElement;
            var nextElement = element;

            //调用_shouldUpdateReactComponent判断是否是更新
            if (_shouldUpdateReactComponent(prevElement, nextElement)) {
                //更新的话直接递归调用子节点的receiveComponent就好了
                prevChild.receiveComponent(nextElement);
                //然后继续使用老的component
                nextChildren[name] = prevChild;
            } else {
                //对于没有老的，那就重新新增一个，重新生成一个component
                var nextChildInstance = instantiateReactComponent(nextElement, null);
                //使用新的component
                nextChildren[name] = nextChildInstance;
            }
        })

        return nextChildren;
    }

    //_diff用来递归找出差别,组装差异对象,添加到更新队列diffQueue。
    ReactDOMComponent.prototype._diff = function(diffQueue, nextChildrenElements) {
        var self = this;
        //拿到之前的子节点的 component类型对象的集合,这个是在刚开始渲染时赋值的，记不得的可以翻上面
        //_renderedChildren 本来是数组，我们搞成map
        var prevChildren = flattenChildren(self._renderedChildren);
        //生成新的子节点的component对象集合，这里注意，会复用老的component对象
        var nextChildren = generateComponentChildren(prevChildren, nextChildrenElements);
        //重新赋值_renderedChildren，使用最新的。
        self._renderedChildren = []
        $.each(nextChildren, function(key, instance) {
            self._renderedChildren.push(instance);
        })


        var nextIndex = 0; //代表到达的新的节点的index
        //通过对比两个集合的差异，组装差异节点添加到队列中
        for (name in nextChildren) {
            if (!nextChildren.hasOwnProperty(name)) {
                continue;
            }
            var prevChild = prevChildren && prevChildren[name];
            var nextChild = nextChildren[name];
            //相同的话，说明是使用的同一个component,所以我们需要做移动的操作
            if (prevChild === nextChild) {
                //添加差异对象，类型：MOVE_EXISTING
                diffQueue.push({
                    parentId: self._rootNodeId,
                    parentNode: $('[data-reactid=' + self._rootNodeId + ']'),
                    type: UPATE_TYPES.MOVE_EXISTING,
                    fromIndex: prevChild._mountIndex,
                    toIndex: nextIndex
                })
            } else { //如果不相同，说明是新增加的节点
                //但是如果老的还存在，就是element不同，但是component一样。我们需要把它对应的老的element删除。
                if (prevChild) {
                    //添加差异对象，类型：REMOVE_NODE
                    diffQueue.push({
                        parentId: self._rootNodeId,
                        parentNode: $('[data-reactid=' + self._rootNodeId + ']'),
                        type: UPATE_TYPES.REMOVE_NODE,
                        fromIndex: prevChild._mountIndex,
                        toIndex: null
                    })

                    //如果以前已经渲染过了，记得先去掉以前所有的事件监听，通过命名空间全部清空
                    if (prevChild._rootNodeId) {
                        $(document).undelegate('.' + prevChild._rootNodeId);
                    }

                }
                //新增加的节点，也组装差异对象放到队列里
                //添加差异对象，类型：INSERT_MARKUP
                diffQueue.push({
                    parentId: self._rootNodeId,
                    parentNode: $('[data-reactid=' + self._rootNodeId + ']'),
                    type: UPATE_TYPES.INSERT_MARKUP,
                    fromIndex: null,
                    toIndex: nextIndex,
                    markup: nextChild.mountComponent() //新增的节点，多一个此属性，表示新节点的dom内容
                })
            }
            //更新mount的index
            nextChild._mountIndex = nextIndex;
            nextIndex++;
        }



        //对于老的节点里有，新的节点里没有的那些，也全都删除掉
        for (name in prevChildren) {
            if (prevChildren.hasOwnProperty(name) && !(nextChildren && nextChildren.hasOwnProperty(name))) {
                //添加差异对象，类型：REMOVE_NODE
                diffQueue.push({
                    parentId: self._rootNodeId,
                    parentNode: $('[data-reactid=' + self._rootNodeId + ']'),
                    type: UPATE_TYPES.REMOVE_NODE,
                    fromIndex: prevChild._mountIndex,
                    toIndex: null
                })
                //如果以前已经渲染过了，记得先去掉以前所有的事件监听
                if (prevChildren[name]._rootNodeId) {
                    $(document).undelegate('.' + prevChildren[name]._rootNodeId);
                }
            }
        }
    };

    //用于将childNode插入到指定位置
    function insertChildAt(parentNode, childNode, index) {
        var beforeChild = parentNode.children().get(index);
        beforeChild ? childNode.insertBefore(beforeChild) : childNode.appendTo(parentNode);
    }

    ReactDOMComponent.prototype._patch = function(updates) {
        var update;
        var initialChildren = {};
        var deleteChildren = [];
        for (var i = 0; i < updates.length; i++) {
            update = updates[i];
            if (update.type === UPATE_TYPES.MOVE_EXISTING || update.type === UPATE_TYPES.REMOVE_NODE) {
                var updatedIndex = update.fromIndex;
                var updatedChild = $(update.parentNode.children().get(updatedIndex));
                var parentID = update.parentID;

                //所有需要更新的节点都保存下来，方便后面使用
                initialChildren[parentID] = initialChildren[parentID] || [];
                //使用parentID作为简易命名空间
                initialChildren[parentID][updatedIndex] = updatedChild;


                //所有需要修改的节点先删除,对于move的，后面再重新插入到正确的位置即可
                deleteChildren.push(updatedChild)
            }

        }

        //删除所有需要先删除的
        $.each(deleteChildren, function(index, child) {
            $(child).remove();
        })


        //再遍历一次，这次处理新增的节点，还有修改的节点这里也要重新插入
        for (var k = 0; k < updates.length; k++) {
            update = updates[k];
            switch (update.type) {
                case UPATE_TYPES.INSERT_MARKUP:
                    insertChildAt(update.parentNode, $(update.markup), update.toIndex);
                    break;
                case UPATE_TYPES.MOVE_EXISTING:
                    insertChildAt(update.parentNode, initialChildren[update.parentID][update.fromIndex], update.toIndex);
                    break;
                case UPATE_TYPES.REMOVE_NODE:
                    // 什么都不需要做，因为上面已经帮忙删除掉了
                    break;
            }
        }
    };

    var ReactCompositeComponent = function (element) {
        this._currentElement = element;
        this._rootNodeId = null;
        this._instance = null;
    };

    ReactCompositeComponent.prototype.mountComponent = function (rootId) {
        this._rootNodeId = rootId;
        var publicProps = this._currentElement.props;
        var ReactClass = this._currentElement.type;
        var inst = new ReactClass(publicProps);
        this._instance = inst;
        inst._reactInternalInstance = this;
        inst.componentWillMount && inst.componentWillMount();
        var renderedElement = inst.render();
        var renderedComponentInstance = instantiateReactComponent(renderedElement);
        var renderedMarkup = renderedComponentInstance.mountComponent(rootId);
        $(document).on('mountReady', function () {
            inst.componentDidMount && inst.componentDidMount();
        });
        this._renderedComponent = this;
        return renderedMarkup;
    };

    ReactCompositeComponent.prototype.receiveComponent = function (nextElement, newState) {
        this._currentElement = nextElement || this._currentElement;
        var inst = this._instance;
        var nextState = $.extend(inst.state, newState);
        var nextProps = this._currentElement.props;
        inst.state = nextState;
        if (inst.shouldComponentUpdate && inst.shouldComponentUpdate(nextProps, nextState) === false)
            return;
        inst.componentWillUpdate && inst.componentWillUpdate(nextProps, nextState);
        var prevComponentInstance = this._renderedComponent;
        var prevRenderedElement = prevComponentInstance._currentElement;
        var nextRenderedElement = this._instance.render();
        if (_shouldUpdateReactComponent(prevRenderedElement, nextRenderedElement)) {
            prevComponentInstance.receiveComponent(this._currentElement);
            inst.componentDidUpdate && inst.componentDidUpdate();
        } else {
            var thisID = this._rootNodeId;
            this._renderedComponent = instantiateReactComponent(this._currentElement);
            var nextMarkup = this._renderedComponent.mountComponent(thisID);
            $('[data-reactid="' + this._rootNodeId + '"]').replaceWith(nextMarkup);
        }
    };

    var instantiateReactComponent = function (node) {
        if (typeof node === 'string' || typeof node === 'number') {
            return new ReactDOMTextComponent(node);
        }
        if (typeof node === 'object' && typeof node.type === 'string') {
            return new ReactDOMComponent(node);
        }
        if (typeof node === 'object' && typeof node.type === 'function') {
            return new ReactCompositeComponent(node);
        }
    };

    var _shouldUpdateReactComponent = function (prevElement, nextElement) {
        if (prevElement != null && nextElement != null) {
            var prevType = typeof prevElement;
            var nextType = typeof nextElement;
            if (prevType === 'string' || prevType === 'number') {
                return nextType === 'string' || nextType === 'number';
            } else {
                return nextType === 'object' && prevElement.type === nextElement.type;
            }
        }
        return false;
    };
    /* === */

    g.React = {
        nextReactRootIndex: 0,
        createElement: function (type, config, children) {
            var props = {}, propName;
            config = config || {};
            var key = config.key || null;
            for (propName in config) {
                if (config.hasOwnProperty(propName)) {
                    props[propName] = config[propName];
                }
            }
            var childrenLength = arguments.length - 2;
            if (childrenLength === 1) {
                props.children = [children];
            } else if (childrenLength >= 1) {
                var childArray = Array(childrenLength);
                for (var i = 0; i < childrenLength; ++i) {
                    childArray[i] = arguments[i + 2];
                }
                props.children = childArray;
            }
            return new ReactElement(type, key, props);
        },
        createClass: function (spec) {
            var constructor = function (props) {
                this.props = props;
                this.state = this.getInitialState();
            };
            constructor.prototype = new ReactClass();
            constructor.prototype.constructor = constructor;
            $.extend(constructor.prototype, spec);
            return constructor;
        },
        render: function (element, container) {
            var component = instantiateReactComponent(element);
            var markup = component.mountComponent(React.nextReactRootIndex++);
            $(container).html(markup);
            $(document).trigger('mountReady');
        }
    };
})(window);