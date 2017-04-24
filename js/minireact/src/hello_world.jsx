const R = function (b, f) {
    if (b) {
        if (f === undefined) {
            b();
        } else {
            f();
        }
    }
};
const E = document.getElementById.bind(document);
// hello world
R(function () {
    React.render('hello world', E('hello-world'));
});
// click
R(function () {
    React.render(
        <div id="test" onclick={() => alert('hello')}>click me</div>,
        E('click-div')
    );
});
// custom elem
R(function () {
    var HelloMessage = React.createClass({
        getInitialState: function () {
            return { type: 'say:' };
        },
        componentWillMount: function () {
            console.log('我就要开始渲染了。。。');
        },
        componentDidMount: function () {
            console.log('我已经渲染好了。。。');
        },
        render: function () {
            return React.createElement('div', null, this.state.type, 'Hello ', this.props.name);
        }
    });
    // React.render(React.createElement(HelloMessage, { name: 'John' }), document.getElementById('custom-elem'));
    React.render(
        <HelloMessage name="John" />,
        E('custom-elem')
    );
});
R(function () {
    var HelloMessage = React.createClass({
        getInitialState: function () {
            return { type: 'say: ' };
        },
        render: function () {
            return (
                <div onclick={() => this.setState({ type: 'shout: ' })}>
                    {this.state.type}Hello {this.props.name}
                </div>
            );
        }
    });
    React.render(
        <HelloMessage name="John" />,
        E('change-state')
    );
});
