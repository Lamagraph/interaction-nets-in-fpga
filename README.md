# Interaction nets based processor in Clash

## Required tools

### Pre-commit

We use [pre-commit](https://pre-commit.com/) for general tidy up of files.
To install pre-commit run:

```shell
pip install pre-commit # or install using your distro package manager
pre-commit install
```

To run pre-commit on all files run

```shell
pre-commit run --all-files
```

### Fourmolu

We use [Fourmolu](https://fourmolu.github.io/) as a formatter for Haskell source files with our custom config.
**Fourmolu must be explicitly enabled in VS Code!**

## Editor

Our editor of choice is [VS Code](https://code.visualstudio.com/) with following extensions:

- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)

## Interaction nets

We use a variation of interaction nets that was proposer by Yves Lafont in 1989 in the paper ["Interaction nets"](https://dl.acm.org/doi/10.1145/96709.96718) as a base for our project.
Below we introduce basic definitions and discuss some assumptions that we use.

Let $\Sigma$ is an alphabet of **agents**.
Let $Ar: \Sigma \to \mathbb{N}$ is an **arity function**.
We suppose that $0 \in \mathbb{N}$.
Each agent $l \in \Sigma$ has a one **primary port** and $Ar(l)$ **secondary ports**: $Ar(l) + 1$ ports in total.

**Network** $\mathcal{n}$ over alphabet $\Sigma$ is an undirected graph where
- Each vertex is labelled with an agent and contains respective number of ports.
- Each edge is a connection between ports.
- Each port can be connected with not more then one port.

$\mathcal{N}_{\Sigma}$ is a set of all possible networks over $\Sigma$.

**Note**
- Network can contains ports that do not connected to other ports. The port without connection is a **free port**. $\mathcal{I}(\mathcal{n})$ is set of free ports of network $\mathcal{n}$ or an **interface of the network $\mathcal{n}$**.
- Network can consists of edges only. In this case, each end of edge is a free port.
- Network without vertices and edges is an **empty network**.

The pair of nodes $a$ and $b$ that connected via primary ports (there is an edge that connects primary port of $a$ with primary port of $b$) is an **active pair**.
We use $a \bowtie b$ to denote that $a$ and $b$ is an active pair.

Network $\mathcal{n}$ is in **normal form** if there is no active pairs in $\mathcal{n}$.

**Reduction rule** $r \in \Sigma \times \Sigma \times \mathcal{N}_{\Sigma}$ is graph rewriting rule.
$\mathcal{R}$ is a set of reduction rules.
- If $(l_1,l_2,\mathcal{n}) \in \mathcal{R}$ then $(l_2, l_1,\mathcal{n}) \in \mathcal{R}$.
- If $(l_1,l_2,\mathcal{n}_1) \in \mathcal{R}$ and $(m_1, m_2,\mathcal{n}_2) \in \mathcal{R}$ then $(l_1,l_2) \neq (m_1,m_2)$.
- For all $(l_1,l_2,\mathcal{n}) \in \mathcal{R}$, $\mathcal{n}$ is in normal form.
- For all $(l_1,l_2,\mathcal{n}) \in \mathcal{R}$, $Ar(l_1) + Ar(l_2) = |\mathcal{I}(\mathcal{n})|$.

Computation is an application of rewriting rules to active pairs.
If there is an active pair $a \bowtie b$ in network $\mathcal{n_0}$, where
- $a$ is labelled with $l_1$
- $b$ is labelled with $l_2$
- $r = (l_1, l_2, \mathcal{m}) \in \mathcal{R}$

then we can replace $a \bowtie b$ with $\mathcal{m}$ and get new network $\mathcal{n_1}$.
Thus, computation is a sequence of steps of the form $\mathcal{n_i} \xrightarrow{r} \mathcal{n_{i+1}}$.
Computation finishes when network in normal form.

Active pair $a \bowtie b$ is **realizable** if there is a sequence

$$\mathcal{n_0} \xrightarrow{r_0} \ldots \xrightarrow{r_{k-1}} \mathcal{n_k},$$

$r_i \in \mathcal{R}$, such that $\mathcal{n}_k$ contains $a \bowtie b$.
We assume that for the given network $\mathcal{n}$ and rules set $\mathcal{R}$, $\mathcal{R}$ contains rules for all **realizable** active pairs.
